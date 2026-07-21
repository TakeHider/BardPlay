using BardPlay2.Core.Keyboard;

namespace BardPlay2.Core;

/// <summary>
/// 現在押されている(=キーダウンしたままの)ノートとKeyStrokeの対応を管理する。
/// 仕様書 9章「押下キー管理」に対応。
///
/// FF14  : モノフォニック制御(直前のノートを判定するために使用)
/// Blue Protocol : 現在押されているキー一覧の管理(ポリフォニックで重複押下/離し忘れを防ぐ)
/// </summary>
public sealed class PressedKeyManager
{
    private readonly Dictionary<int, KeyStroke> _pressed = new();

    public int Count => _pressed.Count;

    public void Add(int note, KeyStroke key) => _pressed[note] = key;

    public bool Remove(int note) => _pressed.Remove(note);

    public bool Remove(int note, out KeyStroke key) => _pressed.Remove(note, out key);

    public bool Contains(int note) => _pressed.ContainsKey(note);

    public bool TryGet(int note, out KeyStroke key) => _pressed.TryGetValue(note, out key);

    public IReadOnlyCollection<int> PressedNotes => _pressed.Keys;

    /// <summary>押されたままになっているキーをすべて離し、管理リストを空にする。</summary>
    public void ReleaseAll(IKeyboardSender sender)
    {
        foreach (var kvp in _pressed)
        {
            sender.KeyUp(kvp.Value);
        }
        _pressed.Clear();
    }
}
