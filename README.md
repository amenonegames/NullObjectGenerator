# NullObjectGenerator

NullObjectを自動生成するSourceGeneratorを作りました！
アトリビュートをつけて、以下のようなクラスや、

```csharp
using NullObjectGenerator;

    [InheritsToNullObj]
    public class TestClass : IHogeInterface , IFugaInterface
    {
        //略
    }
```

以下のようなインターフェースを作ると、自動でNullObjectを生成します。

```csharp
using NullObjectGenerator;

    [InterfaceToNullObj]
    public interface IHogeInterface
    {
        //略
    }
```

また、アトリビュートのコンストラクタを指定すると、
ログの出し分けが可能です。

```csharp
    public enum LogType
    {
        None,     //デフォルト値　ログを出さない
        DebugLog, // UnityEngine.Debug.Logを出す
        DebugLogErr,  //UnityEngine.Debug.LogErrorを出す
        DebugLogWarn, //UnityEngine.Debug.LogWarningを出す
        ThrowException, //System.ExceptionをThrowする
    }
```

詳細な利用方法は以下URLをご覧ください！


https://qiita.com/amenone_games/private/f918ce9b828993484dbb
