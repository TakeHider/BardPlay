using BardPlay2.Core.Logging;
using static BardPlay2.Core.Keyboard.NativeMethods;
using static BardPlay2.Core.Keyboard.VirtualKeyTable;

namespace BardPlay2.Core.Keyboard;

/// <summary>
/// SendInput APIを使ったキー送信(推奨方式)。
/// OSのグローバル入力キューへ直接キーイベントを積むため、送信先ウインドウの指定が不要。
/// 仕様書 3章「対応方式 - SendInput（推奨）」に対応。
/// </summary>
public sealed class SendInputKeyboardSender : IKeyboardSender
{
    private readonly DebugLogger? _logger;

    public SendInputKeyboardSender(DebugLogger? logger = null)
    {
        _logger = logger;
    }

    public string Name => "SendInput";

    public void BeginSession()
    {
        // SendInputは送信先を意識しないため、セッション開始時に行うことはない。
    }

    public void KeyDown(KeyStroke key)
    {
        var inputs = new List<INPUT>(4);
        if (key.Ctrl) inputs.Add(BuildKeyboardInput(VK_CONTROL, false));
        if (key.Alt) inputs.Add(BuildKeyboardInput(VK_MENU, false));
        if (key.Shift) inputs.Add(BuildKeyboardInput(VK_SHIFT, false));
        inputs.Add(BuildKeyboardInput(key.VirtualKey, false));

        Send(inputs);
        _logger?.LogFormat("SendInput KEYDOWN VK={0:X2} ({1})", key.VirtualKey, key);
    }

    public void KeyUp(KeyStroke key)
    {
        // 離す時は押した時と逆順(主キー→Shift→Alt→Ctrl)にするのが安全。
        var inputs = new List<INPUT>(4)
        {
            BuildKeyboardInput(key.VirtualKey, true)
        };
        if (key.Shift) inputs.Add(BuildKeyboardInput(VK_SHIFT, true));
        if (key.Alt) inputs.Add(BuildKeyboardInput(VK_MENU, true));
        if (key.Ctrl) inputs.Add(BuildKeyboardInput(VK_CONTROL, true));

        Send(inputs);
        _logger?.LogFormat("SendInput KEYUP   VK={0:X2} ({1})", key.VirtualKey, key);
    }

    private static void Send(List<INPUT> inputs)
    {
        if (inputs.Count == 0) return;
        var array = inputs.ToArray();
        var size = System.Runtime.InteropServices.Marshal.SizeOf<INPUT>();
        SendInput((uint)array.Length, array, size);
    }

    private static INPUT BuildKeyboardInput(byte virtualKey, bool isKeyUp)
    {
        ushort scanCode = (ushort)(MapVirtualKey(virtualKey, MAPVK_VK_TO_VSC) & 0xFF);

        return new INPUT
        {
            type = INPUT_KEYBOARD,
            U = new InputUnion
            {
                ki = new KEYBDINPUT
                {
                    wVk = 0, // スキャンコード方式で送るのでVKは0にし、KEYEVENTF_SCANCODEを使う
                    wScan = scanCode,
                    dwFlags = KEYEVENTF_SCANCODE | (isKeyUp ? KEYEVENTF_KEYUP : 0),
                    time = 0,
                    dwExtraInfo = IntPtr.Zero
                }
            }
        };
    }
}
