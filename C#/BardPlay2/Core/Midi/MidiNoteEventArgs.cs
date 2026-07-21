namespace BardPlay2.Core.Midi;

/// <summary>
/// NOTE_ON / NOTE_OFF イベントの引数。
/// 仕様書 8章「MIDIイベント」に対応。
/// </summary>
public sealed class MidiNoteEventArgs : EventArgs
{
    /// <summary>ノート番号 (0-127)</summary>
    public int Note { get; }

    /// <summary>ベロシティ (0-127)。NOTE_OFFの時は0。</summary>
    public int Velocity { get; }

    public MidiNoteEventArgs(int note, int velocity)
    {
        Note = note;
        Velocity = velocity;
    }
}
