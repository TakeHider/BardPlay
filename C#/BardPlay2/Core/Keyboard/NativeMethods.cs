using System.Runtime.InteropServices;

namespace BardPlay2.Core.Keyboard;

/// <summary>
/// キーボード入力送信に必要なWin32 API群。
/// SendInput / SendMessage / PostMessage の3方式すべてで利用する。
/// </summary>
internal static class NativeMethods
{
    public const int WM_KEYDOWN = 0x0100;
    public const int WM_KEYUP = 0x0101;

    public const int INPUT_KEYBOARD = 1;

    public const uint KEYEVENTF_EXTENDEDKEY = 0x0001;
    public const uint KEYEVENTF_KEYUP = 0x0002;
    public const uint KEYEVENTF_SCANCODE = 0x0008;

    public const uint MAPVK_VK_TO_VSC = 0x00;

    [StructLayout(LayoutKind.Sequential)]
    public struct KEYBDINPUT
    {
        public ushort wVk;
        public ushort wScan;
        public uint dwFlags;
        public uint time;
        public IntPtr dwExtraInfo;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct MOUSEINPUT
    {
        public int dx;
        public int dy;
        public uint mouseData;
        public uint dwFlags;
        public uint time;
        public IntPtr dwExtraInfo;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct HARDWAREINPUT
    {
        public uint uMsg;
        public ushort wParamL;
        public ushort wParamH;
    }

    // MOUSEINPUT / KEYBDINPUT / HARDWAREINPUT は共用体(union)として重なる。
    [StructLayout(LayoutKind.Explicit)]
    public struct InputUnion
    {
        [FieldOffset(0)] public MOUSEINPUT mi;
        [FieldOffset(0)] public KEYBDINPUT ki;
        [FieldOffset(0)] public HARDWAREINPUT hi;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct INPUT
    {
        public uint type;
        public InputUnion U;
    }

    [DllImport("user32.dll", SetLastError = true)]
    public static extern uint SendInput(uint nInputs, INPUT[] pInputs, int cbSize);

    [DllImport("user32.dll")]
    public static extern uint MapVirtualKey(uint uCode, uint uMapType);

    [DllImport("user32.dll", SetLastError = true)]
    public static extern IntPtr GetForegroundWindow();

    [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
    public static extern IntPtr SendMessage(IntPtr hWnd, int msg, IntPtr wParam, IntPtr lParam);

    [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    public static extern bool PostMessage(IntPtr hWnd, int msg, IntPtr wParam, IntPtr lParam);

    /// <summary>
    /// WM_KEYDOWN / WM_KEYUP 用の lParam を Windows仕様どおりに組み立てる。
    /// 仕様書 6章「WM_KEYUP」に対応。
    /// bit 0-15  : リピートカウント
    /// bit 16-23 : スキャンコード
    /// bit 24    : 拡張キーフラグ
    /// bit 30    : 直前のキー状態 (KeyUpの時は1)
    /// bit 31    : 遷移状態          (KeyUpの時は1)
    /// </summary>
    public static IntPtr BuildLParam(byte virtualKey, bool isKeyUp, bool extended = false)
    {
        uint scanCode = MapVirtualKey(virtualKey, MAPVK_VK_TO_VSC) & 0xFF;

        uint lParam = 1u; // repeat count = 1
        lParam |= scanCode << 16;
        if (extended) lParam |= 1u << 24;
        if (isKeyUp)
        {
            lParam |= 1u << 30; // previous key state = down
            lParam |= 1u << 31; // transition state = up
        }

        return (IntPtr)unchecked((int)lParam);
    }
}
