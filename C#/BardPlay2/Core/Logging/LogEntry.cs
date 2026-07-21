namespace BardPlay2.Core.Logging;

/// <summary>
/// デバッグ画面に表示する1行分の情報。
/// 仕様書 12章「デバッグ機能」の表示例に対応。
/// </summary>
public sealed class LogEntry
{
    public DateTime Timestamp { get; }
    public string Message { get; }

    public LogEntry(string message)
    {
        Timestamp = DateTime.Now;
        Message = message;
    }

    public override string ToString() => $"[{Timestamp:HH:mm:ss.fff}] {Message}";
}
