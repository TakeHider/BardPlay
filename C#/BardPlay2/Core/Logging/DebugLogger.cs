namespace BardPlay2.Core.Logging;

/// <summary>
/// デバッグログの発行元。ON/OFFを切り替え可能で、ログ保存にも対応する。
/// 仕様書 12章「デバッグ機能」に対応。
/// </summary>
public sealed class DebugLogger
{
    private readonly object _fileLock = new();
    private StreamWriter? _fileWriter;

    /// <summary>デバッグ出力が有効かどうか</summary>
    public bool IsEnabled { get; set; }

    /// <summary>ログが1行発行されるたびに通知される(UIスレッドへのマーシャリングは呼び出し側で行う)</summary>
    public event EventHandler<LogEntry>? Logged;

    /// <summary>ログをファイルへ保存する(既に開いている場合は閉じてから開き直す)</summary>
    public void StartFileLogging(string filePath)
    {
        StopFileLogging();
        lock (_fileLock)
        {
            _fileWriter = new StreamWriter(filePath, append: true) { AutoFlush = true };
        }
    }

    public void StopFileLogging()
    {
        lock (_fileLock)
        {
            _fileWriter?.Dispose();
            _fileWriter = null;
        }
    }

    public void Log(string message)
    {
        if (!IsEnabled) return;

        var entry = new LogEntry(message);
        Logged?.Invoke(this, entry);

        lock (_fileLock)
        {
            _fileWriter?.WriteLine(entry.ToString());
        }
    }

    public void LogFormat(string format, params object?[] args) => Log(string.Format(format, args));
}
