using BardPlay2.Core.Logging;
using static BardPlay2.Core.Keyboard.NativeMethods;
using static BardPlay2.Core.Keyboard.VirtualKeyTable;

namespace BardPlay2.Core.Keyboard;

/// <summary>
/// PostMessage APIで WM_KEYDOWN / WM_KEYUP を送る互換方式。
/// SendMessageと異なり非同期(送信先の処理待ちをしない)。
/// 仕様書 3章「対応方式 - PostMessage（互換）」および 6章「WM_KEYUP」に対応。
///
/// Blue Protocol : スターレゾナンス のように、正しいlParamでないとNOTE_OFFが
/// 認識されないゲームがあることが実機で確認されているため、
/// SendMessage版と同じ BuildLParam() を使ってlParamを組み立てる。
/// </summary>
public sealed class PostMessageKeyboardSender : IKeyboardSender
{
    private readonly DebugLogger? _logger;
    private IntPtr _targetWindow = IntPtr.Zero;

    public PostMessageKeyboardSender(DebugLogger? logger = null)
    {
        _logger = logger;
    }

    public string Name => "PostMessage";

    public void BeginSession() => _targetWindow = IntPtr.Zero;

    public void KeyDown(KeyStroke key)
    {
        EnsureTargetWindow();

        if (key.Ctrl) SendKey(VK_CONTROL, false);
        if (key.Alt) SendKey(VK_MENU, false);
        if (key.Shift) SendKey(VK_SHIFT, false);
        SendKey(key.VirtualKey, false);
    }

    public void KeyUp(KeyStroke key)
    {
        EnsureTargetWindow();

        SendKey(key.VirtualKey, true);
        if (key.Shift) SendKey(VK_SHIFT, true);
        if (key.Alt) SendKey(VK_MENU, true);
        if (key.Ctrl) SendKey(VK_CONTROL, true);
    }

    private void EnsureTargetWindow()
    {
        if (_targetWindow == IntPtr.Zero)
            _targetWindow = GetForegroundWindow();
    }

    private void SendKey(byte virtualKey, bool isKeyUp)
    {
        int msg = isKeyUp ? WM_KEYUP : WM_KEYDOWN;
        IntPtr lParam = BuildLParam(virtualKey, isKeyUp);

        PostMessage(_targetWindow, msg, (IntPtr)virtualKey, lParam);

        _logger?.LogFormat("PostMessage hwnd=0x{0:X} {1} VK={2:X2} lParam=0x{3:X8}",
            _targetWindow.ToInt64(), isKeyUp ? "WM_KEYUP" : "WM_KEYDOWN", virtualKey, (uint)lParam.ToInt32());
    }
}
