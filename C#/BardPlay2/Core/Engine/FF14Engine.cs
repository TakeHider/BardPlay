using BardPlay2.Core.Config;
using BardPlay2.Core.Keyboard;
using BardPlay2.Core.Logging;

namespace BardPlay2.Core.Engine;

/// <summary>
/// FINAL FANTASY XIV 用の演奏エンジン。
/// 仕様書 5章「FF14」に対応: モノフォニック / 同時発音不可。
///
/// NOTE_ON: 以前押されているキーがあればNOTE_OFF送信 → 新しいキーを押す
/// NOTE_OFF: 押しているキーだけ離す
/// </summary>
public sealed class FF14Engine : GameEngineBase
{
    private int? _currentNote; // 現在キーダウン中のノート番号(トランスポーズ適用後)

    public FF14Engine(GameProfile profile, IKeyboardSender sender, DebugLogger? logger = null)
        : base(profile, sender, logger)
    {
    }

    public override void NoteOn(int note, int velocity)
    {
        if (!TryResolveKeyStroke(note, out var mappedNote, out var keyStroke))
        {
            Logger?.LogFormat("NOTE_ON {0} は演奏範囲外のためスキップ", note);
            return;
        }

        // 既に別のノートが鳴っていたら、まずそれを離す(モノフォニックのため同時発音しない)
        if (_currentNote.HasValue && _currentNote.Value != mappedNote)
        {
            if (PressedKeys.Remove(_currentNote.Value, out var previousKey))
            {
                Sender.KeyUp(previousKey);
            }
        }

        if (_currentNote != mappedNote)
        {
            Sender.KeyDown(keyStroke);
            PressedKeys.Add(mappedNote, keyStroke);
            _currentNote = mappedNote;

            Logger?.LogFormat("NOTE_ON {0} -> {1}", NoteName(note), keyStroke);
        }
    }

    public override void NoteOff(int note)
    {
        if (!TryResolveKeyStroke(note, out var mappedNote, out _))
        {
            Logger?.LogFormat("NOTE_OFF {0} は演奏範囲外のためスキップ", note);
            return;
        }

        // 今押しているノートと同じ時だけ離す(FF14はモノフォニックなので他のノートは無視)
        if (_currentNote == mappedNote && PressedKeys.Remove(mappedNote, out var keyStroke))
        {
            Sender.KeyUp(keyStroke);
            _currentNote = null;

            Logger?.LogFormat("NOTE_OFF {0} -> {1}", NoteName(note), keyStroke);
        }
        else
        {
            Logger?.LogFormat("NOTE_OFF {0} (mapped={1}) は現在の押下キーと一致しないため無視 (currentNote={2})",
                NoteName(note), mappedNote, _currentNote?.ToString() ?? "none");
        }
    }

    public override void ReleaseAll()
    {
        base.ReleaseAll();
        _currentNote = null;
    }
}
