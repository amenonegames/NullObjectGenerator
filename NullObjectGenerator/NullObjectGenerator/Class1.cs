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
    sealed class NullObjAttribute : Attribute
    {
    
        public NullObjAttribute()
        {
        }
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
            var result = GetMembers(receiver, context);

            
            foreach (var data in result)
            {
                var type = data.classDeclarationSyntax;
                var typeSymbol = context.Compilation.GetSemanticModel(type.SyntaxTree).GetDeclaredSymbol(type);
                if (typeSymbol == null) throw new Exception("can not get typeSymbol.");

                var source = SourceText.From(GenerateType( typeSymbol , data),Encoding.UTF8);
                
                var filename =
                    $"{typeSymbol.Name}AsNullObject.cs";
                    
                context.AddSource(filename, source);
            }
        }

        private string GenerateType(ISymbol symbol, ClassData data)
        {
            
            List<string> allUsings = new List<string>();
            foreach (var iDataInterface in data.interfaces)
            {
                allUsings.AddRange(GetUsingDirectives(iDataInterface));
            }

            var usings = allUsings.Distinct();
            

            var classAccessiblity = symbol.DeclaredAccessibility.ToString().ToLower();
            var namespaceName = symbol.ContainingNamespace.IsGlobalNamespace
                ? ""
                : $@"namespace {symbol.ContainingNamespace.ToDisplayString()}
{{";
            var classDeclaration = $@"
    {classAccessiblity} class {symbol.Name}AsNullObj
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

            foreach (var proprety in data.properties)
            {
                var accessiblity = proprety.DeclaredAccessibility.ToString().ToLower();
                
                sb.Append($@"
                {accessiblity} {proprety.Type} {proprety.Name} 
                {{ ");
                if (proprety.GetMethod != null)
                {
                    sb.Append($@"get; ");
                }
                if (proprety.SetMethod != null)
                {
                    sb.Append($@"set; ");
                }
                sb.Append($@"
                }}");
            }
            

            foreach (var method in data.methods)
            {
                var accessiblity = method.DeclaredAccessibility.ToString().ToLower();
                sb.Append($@"
                {accessiblity} {method.ReturnType} {method.Name}({string.Join(",", method.Parameters.Select(x => $"{x.Type} {x.Name}"))})
                {{");

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

        private List<ClassData> GetMembers(SyntaxReceiver receiver , GeneratorExecutionContext context)
        {
            List<ClassData> result = new List<ClassData>();
            
            foreach (var classSyntax in receiver.classes)
            {
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
                                data.methods.Add(method);
                                break;

                            case IPropertySymbol property:
                                // プロパティの処理
                                data.properties.Add(property);
                                break;
                            
                        }

                    }
                }
                
                result.Add(data);
            }

            return result;
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
        
        public class ClassData
        {
            public ClassDeclarationSyntax classDeclarationSyntax;
            public List<IMethodSymbol> methods =new List<IMethodSymbol>();
            public List<IPropertySymbol> properties = new List<IPropertySymbol>();
            public List<INamedTypeSymbol> interfaces = new List<INamedTypeSymbol>();

            public ClassData(ClassDeclarationSyntax classDeclarationSyntax)
            {
                this.classDeclarationSyntax = classDeclarationSyntax;
            } 
        }


        class SyntaxReceiver : ISyntaxReceiver
        {
            public List<ClassDeclarationSyntax> classes { get; } = new List<ClassDeclarationSyntax>();

            public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
            {
                if (syntaxNode is ClassDeclarationSyntax  s && s.AttributeLists.Count > 0)
                {
                    var attr = s.AttributeLists.SelectMany(x => x.Attributes)
                        .FirstOrDefault(x => x.Name.ToString() is "NullObj"|| x.Name.ToString() is "NullObjAttribute");
                    if (attr != null)
                    {
                        classes.Add((s));
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