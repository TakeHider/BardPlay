using BardPlay2.Core.Logging;

namespace BardPlay2.Core.Keyboard;

/// <summary>
/// 送信方式名 ("SendInput" / "SendMessage" / "PostMessage") から
/// IKeyboardSender の実体を作る。ゲームプロファイルの初期値やユーザー選択に使う。
/// 将来方式を追加する場合はここに case を1つ足すだけでよい。
/// </summary>
public static class KeyboardSenderFactory
{
    public static readonly string[] AvailableSenderNames = { "SendInput", "SendMessage", "PostMessage" };

    public static IKeyboardSender Create(string senderName, DebugLogger? logger = null)
    {
        return senderName switch
        {
            "SendInput" => new SendInputKeyboardSender(logger),
            "SendMessage" => new SendMessageKeyboardSender(logger),
            "PostMessage" => new PostMessageKeyboardSender(logger),
            _ => throw new ArgumentException($"未知のキー送信方式です: '{senderName}'", nameof(senderName))
        };
    }
}
