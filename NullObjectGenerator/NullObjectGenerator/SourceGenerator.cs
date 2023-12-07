using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace NullObjectGenerator
{
    [Generator]
    public class SourceGenerator : ISourceGenerator
    {
        public void Initialize(GeneratorInitializationContext context)
        {
            context.RegisterForPostInitialization(x => SetDefaultAttribute(x));
            
            context.RegisterForSyntaxNotifications( () => new SyntaxReceiver() );
        }

        private void SetDefaultAttribute(GeneratorPostInitializationContext context)
        {
            // AutoPropertyAttributeのコード本体
            const string AttributeText = @"
using System;
namespace NullObjectGenerator
{
    [AttributeUsage(AttributeTargets.Class,
                    Inherited = false, AllowMultiple = false)]
    sealed class InheritsToNullObjAttribute : Attribute
    {
        public LogType LogType { get; }
        public InheritsToNullObjAttribute( LogType logType = LogType.None)
        {
            LogType = logType;
        }
    }

    [AttributeUsage(AttributeTargets.Interface,
                    Inherited = false, AllowMultiple = false)]
    sealed class InterfaceToNullObjAttribute : Attribute
    {
        public LogType LogType { get; }
        public InterfaceToNullObjAttribute( LogType logType = LogType.None)
        {
            LogType = logType;
        }
    }

    [Flags]
    internal enum LogType
    {
        None = 0,
        DebugLog = 1,
        DebugLogErr = 1 << 1,
        ThrowException = 1 << 2,
    }
}
";            
            //コンパイル時に参照するアセンブリを追加
            context.AddSource
            (
                "NullObjAttribute.cs",
                SourceText.From(AttributeText,Encoding.UTF8)
            );
        }
        
        public void Execute(GeneratorExecutionContext context)
        {
            // シンタックスレシーバを取得
            var receiver = context.SyntaxReceiver as SyntaxReceiver;
            if (receiver == null) return;
            var classResult = GetClassDatas(receiver, context);
            var interfaceResult = getInterfaceDatas(receiver, context);
            
            foreach (var data in classResult)
            {
                var type = data.classDeclarationSyntax;
                var typeSymbol = context.Compilation.GetSemanticModel(type.SyntaxTree).GetDeclaredSymbol(type);
                if (typeSymbol == null) throw new Exception("can not get typeSymbol.");

                var source = SourceText.From(GenerateClassForInherits( typeSymbol , data),Encoding.UTF8);
                
                var filename =
                    $"{typeSymbol.Name}AsNullObject.g.cs";
                    
                context.AddSource(filename, source);
            }

            foreach (var data in interfaceResult)
            {
                var type = data.interfaceDeclarationSyntax;
                var typeSymbol = context.Compilation.GetSemanticModel(type.SyntaxTree).GetDeclaredSymbol(type);
                if (typeSymbol == null) throw new Exception("can not get typeSymbol.");

                var source = SourceText.From(GenerateClassForInterface( typeSymbol , data),Encoding.UTF8);
                
                var filename =
                    $"{typeSymbol.Name}AsNullObject.g.cs";
                    
                context.AddSource(filename, source);
            }
            
        }

        private string GenerateClassForInherits(ISymbol symbol, ClassData data)
        {

            List<string> allUsings = new List<string>();
            List<string> interfaces = new List<string>();
            foreach (var iDataInterface in data.interfaces)
            {
                allUsings.AddRange(GetUsingDirectives(iDataInterface));
                interfaces.Add(iDataInterface.Name);
            }

            var usings = allUsings.Distinct();

            return GenerateClass(symbol, data , usings, interfaces);
        }
        
        private string GenerateClassForInterface(ISymbol symbol, InterfaceData data)
        {

            // SyntaxTreeのルートを取得（CompilationUnitSyntax）
            var root = data.interfaceDeclarationSyntax.SyntaxTree.GetRoot() as CompilationUnitSyntax;
            if (root == null)
            {
                return "";
            }

            // usingディレクティブを取得
            var usings =  root.Usings.Select(usingDirective => usingDirective.Name.ToString());
            string[] interfaces = new[] { data.interfaceDeclarationSyntax.Identifier.ValueText };

            return GenerateClass(symbol, data , usings , interfaces);
        }
        
        private string GenerateClass(ISymbol symbol, IImplementationData data , IEnumerable<string> usings , IEnumerable<string> interfaces)
        {
            var classAccessiblity = symbol.DeclaredAccessibility.ToString().ToLower();
            var interfacesStr = interfaces.Any() ? $": {string.Join(",", interfaces)}" : "";
            var namespaceName = symbol.ContainingNamespace.IsGlobalNamespace
                ? ""
                : $@"namespace {symbol.ContainingNamespace.ToDisplayString()}
{{";
            var classDeclaration = $@"
    ///
    {classAccessiblity} class {symbol.Name}AsNullObj {interfacesStr}
    {{
";
            var sb = new StringBuilder();

            foreach (var ns in usings)
            {
                sb.Append($@"using {ns};
");
            }
            sb.Append(namespaceName);
            sb.Append(classDeclaration);

            foreach (var proprety in data.Properties)
            {
                var accessiblity = proprety.DeclaredAccessibility.ToString().ToLower();
                
                sb.Append($@"
            {accessiblity} {proprety.Type} {proprety.Name} 
            {{ ");
                if (proprety.GetMethod != null)
                {
                    sb.Append($@"
                get 
                {{ ");
                    AppendLog(sb, data.LogType, $@"
                    {proprety.Name} is null. return default value.");
                    sb.Append($@"
                    return default;
                }}");
                }
                if (proprety.SetMethod != null)
                {
                    sb.Append($@"
                set
                {{
                       ");
                    AppendLog(sb, data.LogType, $@"
                    {proprety.Name} is null. do nothing.");
                    sb.Append(@"
                }");
                }
                sb.Append($@"
            }}");
            }
            

            foreach (var method in data.Methods)
            {
                var accessiblity = method.DeclaredAccessibility.ToString().ToLower();
                sb.Append($@"
            {accessiblity} {method.ReturnType} {method.Name}({string.Join(",", method.Parameters.Select(x => $"{x.Type} {x.Name}"))})
            {{");
                
                AppendLog(sb, data.LogType, $@"
                {method.Name} is null. do nothing.");

                if(method.ReturnType.Name == "UniTask")
                {
                    sb.Append($@"
                return UniTask.CompletedTask;");
                }
                else if(method.ReturnType.SpecialType != SpecialType.System_Void)
                {
                    sb.Append($@"
                return default;");
                }
                
                sb.Append(@"
            }");
                
            }
            
            sb.Append(@"
    }"); // Close class
            if (!symbol.ContainingNamespace.IsGlobalNamespace)
            {
                sb.Append(@"
}"); // Close namespace
            }
            return sb.ToString();
        }

        private List<ClassData> GetClassDatas(SyntaxReceiver receiver , GeneratorExecutionContext context)
        {
            List<ClassData> result = new List<ClassData>();

            foreach (var target in receiver.targetClasses)
            {
                var classSyntax = target.cla;
                
                // SemanticModelを取得
                SemanticModel model = context.Compilation.GetSemanticModel(classSyntax.SyntaxTree);

                // クラスのシンボルを取得
                INamedTypeSymbol classSymbol = model.GetDeclaredSymbol(classSyntax) as INamedTypeSymbol;
                if (classSymbol == null) continue;

                ClassData data = new ClassData(classSyntax);
                
                // クラスが実装しているインターフェースを取得
                foreach (var interfaceSymbol in classSymbol.AllInterfaces)
                {
                    data.interfaces.Add(interfaceSymbol);
                    // インターフェースのメンバーを取得
                    foreach (var member in interfaceSymbol.GetMembers())
                    {

                        switch (member)
                        {
                            
                            case IMethodSymbol method when !method.IsPropertyAccessor():
                                // メソッドの処理
                                data.Methods.Add(method);
                                break;

                            case IPropertySymbol property:
                                // プロパティの処理
                                data.Properties.Add(property);
                                break;
                            
                        }

                    }
                }
                
                var arg = target.attr.ArgumentList.Arguments[0];
                var expr = arg.Expression;
                var parsed = Enum.ToObject(typeof(LogType), model.GetConstantValue(expr).Value);
                data.LogType = (LogType)parsed;
                
                result.Add(data);
            }
            
            return result;
        }

        private List<InterfaceData> getInterfaceDatas(SyntaxReceiver receiver , GeneratorExecutionContext context)
        {
            List<InterfaceData> result = new List<InterfaceData>();

            foreach (var target in receiver.targetInterfaces)
            {
                var interfaceSyntax = target.inte;
                
                // SemanticModelを取得
                SemanticModel model = context.Compilation.GetSemanticModel(interfaceSyntax.SyntaxTree);

                // クラスのシンボルを取得
                INamedTypeSymbol interfaceSymbol = model.GetDeclaredSymbol(interfaceSyntax) as INamedTypeSymbol;
                if (interfaceSymbol == null) continue;

                InterfaceData data = new InterfaceData(interfaceSyntax);

                // インターフェースのメンバーを取得
                foreach (var member in interfaceSymbol.GetMembers())
                {

                    switch (member)
                    {
                        
                        case IMethodSymbol method when !method.IsPropertyAccessor():
                            // メソッドの処理
                            data.Methods.Add(method);
                            break;

                        case IPropertySymbol property:
                            // プロパティの処理
                            data.Properties.Add(property);
                            break;
                        
                    }

                }

                var args = target.attr.ArgumentList.Arguments;
                if (args.Count == 0)
                {
                    data.LogType = LogType.None;
                }
                else
                {
                    var arg = args[0];
                    var expr = arg.Expression;
                    var parsed = Enum.ToObject(typeof(LogType), model.GetConstantValue(expr).Value);
                    data.LogType = (LogType)parsed;
                }
                
                result.Add(data);
            }
            
            return result;
        }
        
        private void AppendLog( StringBuilder sb, LogType logType , string message)
        {
            if (logType.HasFlag(LogType.DebugLog))
            {
                sb.Append($@"
                    UnityEngine.Debug.Log(""{message}"");");
            }
            if (logType.HasFlag(LogType.DebugLogErr))
            {
                sb.Append($@"
                    UnityEngine.Debug.LogError(""{message}"");");
            }
            if (logType.HasFlag(LogType.ThrowException))
            {
                sb.Append($@"
                    throw new Exception(""{message}"");");
            }

        }
        

    
        public IEnumerable<string> GetUsingDirectives(INamedTypeSymbol namedTypeSymbol)
        {
            // namedTypeSymbolの宣言を取得
            var declarations = namedTypeSymbol.DeclaringSyntaxReferences;

            var usingDirectives = new HashSet<string>();

            foreach (var declaration in declarations)
            {
                // SyntaxNodeを取得
                var syntax = declaration.GetSyntax() as SyntaxNode;
                if (syntax == null) continue;

                // SyntaxNodeからCompilationUnitSyntaxを取得
                var root = syntax.SyntaxTree.GetRoot() as CompilationUnitSyntax;
                if (root == null) continue;

                // CompilationUnitSyntax内のusingディレクティブを取得
                foreach (var usingDirective in root.Usings)
                {
                    usingDirectives.Add(usingDirective.Name.ToString());
                }
            }

            return usingDirectives;
        }
        
        public class ClassData : IImplementationData
        {
            public ClassDeclarationSyntax classDeclarationSyntax;
            public LogType LogType { get; set; }
            public List<IMethodSymbol> Methods { get; } =new List<IMethodSymbol>();
            public List<IPropertySymbol> Properties { get; }= new List<IPropertySymbol>();
            public List<INamedTypeSymbol> interfaces = new List<INamedTypeSymbol>();
            
            
            public ClassData(ClassDeclarationSyntax classDeclarationSyntax)
            {
                this.classDeclarationSyntax = classDeclarationSyntax;
            } 
        }
        
        public class InterfaceData : IImplementationData
        {
            public InterfaceDeclarationSyntax interfaceDeclarationSyntax;
            public LogType LogType { get; set; }
            public List<IMethodSymbol> Methods { get; } =new List<IMethodSymbol>();
            public List<IPropertySymbol> Properties { get; }= new List<IPropertySymbol>();
            
            public InterfaceData(InterfaceDeclarationSyntax interfaceDeclarationSyntax)
            {
                this.interfaceDeclarationSyntax = interfaceDeclarationSyntax;
            } 
        }

        public interface IImplementationData
        {
            List<IMethodSymbol> Methods { get; } 
            List<IPropertySymbol> Properties { get; }
            LogType LogType { get; set; }
        }

        class SyntaxReceiver : ISyntaxReceiver
        {
            public List<(ClassDeclarationSyntax cla , AttributeSyntax attr)> targetClasses { get; } = new List<(ClassDeclarationSyntax cla , AttributeSyntax attr)>();
            public List<(InterfaceDeclarationSyntax inte , AttributeSyntax attr)> targetInterfaces { get; } = new List<(InterfaceDeclarationSyntax inte , AttributeSyntax attr)>();

            public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
            {
                if (syntaxNode is ClassDeclarationSyntax  cla && cla.AttributeLists.Count > 0)
                {
                    var attr = cla.AttributeLists.SelectMany(x => x.Attributes)
                        .FirstOrDefault(x => x.Name.ToString() is "InheritsToNullObj"|| x.Name.ToString() is "InheritsToNullObjAttribute");
                    if (attr != null)
                    {
                        targetClasses.Add((cla,attr));
                    }
                }
                else if (syntaxNode is InterfaceDeclarationSyntax inte && inte.AttributeLists.Count > 0)
                {
                    var attr = inte.AttributeLists.SelectMany(x => x.Attributes)
                        .FirstOrDefault(x => x.Name.ToString() is "InterfaceToNullObj"|| x.Name.ToString() is "InterfaceToNullObjAttribute");
                    if (attr != null)
                    {
                        targetInterfaces.Add((inte,attr));
                    }
                }
            }
        }
    }
    
    public static class MethodSymbolExtensions
    {
        public static bool IsPropertyAccessor(this IMethodSymbol methodSymbol)
        {
            return methodSymbol.MethodKind == MethodKind.PropertyGet || methodSymbol.MethodKind == MethodKind.PropertySet;
        }
    }

}