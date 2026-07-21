using BardPlay2.Core.Config;
using BardPlay2.Core.Keyboard;
using BardPlay2.Core.Logging;

namespace BardPlay2.Core.Engine;

/// <summary>
/// BLUE PROTOCOL：スターレゾナンス 用の演奏エンジン。
/// 仕様書 5章「BLUE PROTOCOL」に対応: ポリフォニック / 同時発音可能。
///
/// NOTE_ON: 押すだけ
/// NOTE_OFF: 対象キーだけ離す
///
/// 注意: Blue Protocolでは正しいlParamを設定しないとNOTE_OFFが認識されないことが
/// 実機で確認されているため、既定のキー送信方式は SendMessage / PostMessage を推奨する
/// (プロファイルJSONの DefaultKeyboardSender で設定)。
/// </summary>
public sealed class BlueProtocolEngine : GameEngineBase
{
    public BlueProtocolEngine(GameProfile profile, IKeyboardSender sender, DebugLogger? logger = null)
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

        // 既に押されているキーの再押下は行わない(重複KeyDown防止)
        if (PressedKeys.Contains(mappedNote))
            return;

        Sender.KeyDown(keyStroke);
        PressedKeys.Add(mappedNote, keyStroke);

        Logger?.LogFormat("NOTE_ON {0} -> {1}", NoteName(note), keyStroke);
    }

    public override void NoteOff(int note)
    {
        if (!TryResolveKeyStroke(note, out var mappedNote, out _))
        {
            Logger?.LogFormat("NOTE_OFF {0} は演奏範囲外のためスキップ", note);
            return;
        }

        if (PressedKeys.Remove(mappedNote, out var keyStroke))
        {
            Sender.KeyUp(keyStroke);
            Logger?.LogFormat("NOTE_OFF {0} -> {1}", NoteName(note), keyStroke);
        }
        else
        {
            Logger?.LogFormat("NOTE_OFF {0} (mapped={1}) は押下中のキーとして記録されていないため無視",
                NoteName(note), mappedNote);
        }
    }
}
