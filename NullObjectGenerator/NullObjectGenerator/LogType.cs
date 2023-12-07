using System;

namespace NullObjectGenerator
{
    [Flags]
    public enum LogType
    {
        None = 0,
        DebugLog = 1,
        DebugLogErr = 1 << 1,
        ThrowException = 1 << 2,
    }
}