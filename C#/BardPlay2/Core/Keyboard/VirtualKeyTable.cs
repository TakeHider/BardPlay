namespace BardPlay2.Core.Keyboard;

/// <summary>
/// キー名(文字列)と仮想キーコードの対応表。
/// Delphi版 threadMIDIEvent.pas の setKeyCode() を踏襲しつつ、
/// 修飾キー(shift/ctrl/alt)はKeyStroke側のフラグで扱うため、
/// 単体キー名だけをここで解決する。
/// </summary>
public static class VirtualKeyTable
{
    public const byte VK_SHIFT = 0x10;
    public const byte VK_CONTROL = 0x11;
    public const byte VK_MENU = 0x12; // Alt

    private static readonly Dictionary<string, byte> Table = new(StringComparer.OrdinalIgnoreCase)
    {
        ["backspace"] = 0x08,
        ["tab"] = 0x09,
        ["enter"] = 0x0D,
        ["shift"] = VK_SHIFT,
        ["ctrl"] = VK_CONTROL,
        ["alt"] = VK_MENU,
        ["pause"] = 0x13,
        ["capslock"] = 0x14,
        ["esc"] = 0x1B,
        ["space"] = 0x20,
        ["pageup"] = 0x21,
        ["pagedown"] = 0x22,
        ["end"] = 0x23,
        ["home"] = 0x24,
        ["left"] = 0x25,
        ["up"] = 0x26,
        ["right"] = 0x27,
        ["down"] = 0x28,
        ["printscrn"] = 0x2C,
        ["insert"] = 0x2D,
        ["delete"] = 0x2E,
        ["0"] = 0x30,
        ["1"] = 0x31,
        ["2"] = 0x32,
        ["3"] = 0x33,
        ["4"] = 0x34,
        ["5"] = 0x35,
        ["6"] = 0x36,
        ["7"] = 0x37,
        ["8"] = 0x38,
        ["9"] = 0x39,
        ["@"] = 0x40,
        ["a"] = 0x41,
        ["b"] = 0x42,
        ["c"] = 0x43,
        ["d"] = 0x44,
        ["e"] = 0x45,
        ["f"] = 0x46,
        ["g"] = 0x47,
        ["h"] = 0x48,
        ["i"] = 0x49,
        ["j"] = 0x4A,
        ["k"] = 0x4B,
        ["l"] = 0x4C,
        ["m"] = 0x4D,
        ["n"] = 0x4E,
        ["o"] = 0x4F,
        ["p"] = 0x50,
        ["q"] = 0x51,
        ["r"] = 0x52,
        ["s"] = 0x53,
        ["t"] = 0x54,
        ["u"] = 0x55,
        ["v"] = 0x56,
        ["w"] = 0x57,
        ["x"] = 0x58,
        ["y"] = 0x59,
        ["z"] = 0x5A,
        ["numpad0"] = 0x60,
        ["numpad1"] = 0x61,
        ["numpad2"] = 0x62,
        ["numpad3"] = 0x63,
        ["numpad4"] = 0x64,
        ["numpad5"] = 0x65,
        ["numpad6"] = 0x66,
        ["numpad7"] = 0x67,
        ["numpad8"] = 0x68,
        ["numpad9"] = 0x69,
        ["f1"] = 0x70,
        ["f2"] = 0x71,
        ["f3"] = 0x72,
        ["f4"] = 0x73,
        ["f5"] = 0x74,
        ["f6"] = 0x75,
        ["f7"] = 0x76,
        ["f8"] = 0x77,
        ["f9"] = 0x78,
        ["f10"] = 0x79,
        ["f11"] = 0x7A,
        ["f12"] = 0x7B,
        [";"] = 0xBA,
        ["="] = 0xBB,
        [","] = 0xBC,
        ["-"] = 0xBD,
        ["."] = 0xBE,
        ["/"] = 0xBF,
        ["`"] = 0xC0,
        ["["] = 0xDB,
        ["\\"] = 0xDC,
        ["]"] = 0xDD,
        ["'"] = 0xDE,
    };

    /// <summary>キー名から仮想キーコードを取得する。見つからない場合は false。</summary>
    public static bool TryGetVirtualKey(string keyName, out byte virtualKey) =>
        Table.TryGetValue(keyName.Trim(), out virtualKey);

    /// <summary>キー名から仮想キーコードを取得する。見つからない場合は例外。</summary>
    public static byte GetVirtualKey(string keyName)
    {
        if (!TryGetVirtualKey(keyName, out var vk))
            throw new ArgumentException($"未知のキー名です: '{keyName}'", nameof(keyName));
        return vk;
    }

    public static IReadOnlyCollection<string> KeyNames => Table.Keys;
}
