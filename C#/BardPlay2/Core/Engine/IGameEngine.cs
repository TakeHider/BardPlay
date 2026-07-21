namespace BardPlay2.Core.Engine;

/// <summary>
/// ゲームごとの演奏ルール(モノフォニック/ポリフォニック、キー配置、修飾キーなど)を
/// 集約するインターフェース。
/// 仕様書 5章「演奏エンジン」に対応。
///
/// 新しいゲームに対応する場合は、このインターフェースを実装したクラスを1つ追加し、
/// 対応するJSONプロファイルをProfilesフォルダに置くだけでよい(設計方針4)。
/// </summary>
public interface IGameEngine
{
    /// <summary>ゲーム名(GUI表示・プロファイル名と一致)</summary>
    string Name { get; }

    /// <summary>MIDI NOTE_ONを受信した時の処理</summary>
    void NoteOn(int note, int velocity);

    /// <summary>MIDI NOTE_OFFを受信した時の処理</summary>
    void NoteOff(int note);

    /// <summary>演奏中断時などに、押したままになっているキーをすべて離す</summary>
    void ReleaseAll();
}
