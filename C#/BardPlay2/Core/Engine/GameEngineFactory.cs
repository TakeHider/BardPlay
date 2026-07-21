using BardPlay2.Core.Config;
using BardPlay2.Core.Keyboard;
using BardPlay2.Core.Logging;

namespace BardPlay2.Core.Engine;

/// <summary>
/// GameProfile.EngineId から対応する IGameEngine を組み立てる。
/// 仕様書14章「対応ゲーム追加」: 新しいゲームエンジンを追加する時は、
/// IGameEngine実装クラスを1つ作り、ここに case を足すだけでよい。
/// </summary>
public static class GameEngineFactory
{
    public static IGameEngine Create(GameProfile profile, IKeyboardSender sender, DebugLogger? logger = null)
    {
        return profile.EngineId switch
        {
            "FF14" => new FF14Engine(profile, sender, logger),
            "BlueProtocol" => new BlueProtocolEngine(profile, sender, logger),
            _ => throw new NotSupportedException(
                $"未対応のエンジンIDです: '{profile.EngineId}' (プロファイル: {profile.Name})")
        };
    }
}
