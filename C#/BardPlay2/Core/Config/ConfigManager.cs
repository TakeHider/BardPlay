using System.Text.Json;
using System.Text.Json.Serialization;

namespace BardPlay2.Core.Config;

/// <summary>
/// 設定ファイル(bardplay.config.json)とゲームプロファイル(Profiles\*.json)の
/// 読み書きを担当する。
/// 仕様書 10章「設定保存」に対応。
/// </summary>
public sealed class ConfigManager
{
    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        WriteIndented = true,
        PropertyNameCaseInsensitive = true,
        Converters = { new JsonStringEnumConverter() },
        DictionaryKeyPolicy = null
    };

    private readonly string _configFilePath;
    private readonly string _profilesFolder;

    public ConfigManager(string? baseDirectory = null)
    {
        var baseDir = baseDirectory ?? AppContext.BaseDirectory;
        _configFilePath = Path.Combine(baseDir, "bardplay.config.json");
        _profilesFolder = Path.Combine(baseDir, "Profiles");
    }

    /// <summary>設定ファイルを読み込む。存在しなければ既定値を返す。</summary>
    public AppConfig LoadConfig()
    {
        try
        {
            if (!File.Exists(_configFilePath))
                return new AppConfig();

            var json = File.ReadAllText(_configFilePath);
            return JsonSerializer.Deserialize<AppConfig>(json, JsonOptions) ?? new AppConfig();
        }
        catch (Exception)
        {
            // 破損した設定ファイルなどはアプリを止めず、既定値にフォールバックする
            return new AppConfig();
        }
    }

    public void SaveConfig(AppConfig config)
    {
        var json = JsonSerializer.Serialize(config, JsonOptions);
        File.WriteAllText(_configFilePath, json);
    }

    /// <summary>
    /// Profilesフォルダにある *.json をすべてゲームプロファイルとして読み込む。
    /// 新しいゲームを追加したい時は、このフォルダにJSONを1つ置くだけでよい
    /// (仕様書14章「ゲームプロファイルの外部ファイル化」に対応)。
    /// </summary>
    public List<GameProfile> LoadProfiles()
    {
        var profiles = new List<GameProfile>();

        if (!Directory.Exists(_profilesFolder))
            return profiles;

        foreach (var file in Directory.GetFiles(_profilesFolder, "*.json").OrderBy(f => f))
        {
            try
            {
                var json = File.ReadAllText(file);
                // JSONのキー("60"等)を数値キーのDictionaryとして読むため、
                // 一旦文字列キーで受けてから変換する。
                var raw = JsonSerializer.Deserialize<RawGameProfile>(json, JsonOptions);
                if (raw == null) continue;

                var profile = new GameProfile
                {
                    Name = raw.Name,
                    EngineId = raw.EngineId,
                    PlayMode = raw.PlayMode,
                    DefaultKeyboardSender = raw.DefaultKeyboardSender,
                    KeyMapping = raw.KeyMapping
                        .Where(kv => int.TryParse(kv.Key, out _))
                        .ToDictionary(kv => int.Parse(kv.Key), kv => kv.Value)
                };
                // 妥当性チェック：Name と EngineId が必須
                if (string.IsNullOrWhiteSpace(profile.Name) || string.IsNullOrWhiteSpace(profile.EngineId))
                {
                    // ログ出力またはデバッグ表示してファイルをスキップ
                    continue;
                }
                profiles.Add(profile);
            }
            catch (Exception)
            {
                // 1ファイルの不備で全体を止めない。壊れたプロファイルはスキップする。
            }
        }

        return profiles;
    }

    /// <summary>プロファイルをJSONとして保存する(キーマッピング編集画面などから使用)。</summary>
    public void SaveProfile(GameProfile profile)
    {
        Directory.CreateDirectory(_profilesFolder);

        var raw = new RawGameProfile
        {
            Name = profile.Name,
            EngineId = profile.EngineId,
            PlayMode = profile.PlayMode,
            DefaultKeyboardSender = profile.DefaultKeyboardSender,
            KeyMapping = profile.KeyMapping.ToDictionary(kv => kv.Key.ToString(), kv => kv.Value)
        };

        var fileName = string.Concat(profile.Name.Where(c => !Path.GetInvalidFileNameChars().Contains(c))) + ".json";
        var path = Path.Combine(_profilesFolder, fileName);
        var json = JsonSerializer.Serialize(raw, JsonOptions);
        File.WriteAllText(path, json);
    }

    /// <summary>System.Text.Jsonは非文字列キーのDictionaryを既定でサポートしないため、中継用に文字列キー版を使う。</summary>
    private sealed class RawGameProfile
    {
        public string Name { get; set; } = "";
        public string EngineId { get; set; } = "";
        public PlayMode PlayMode { get; set; } = PlayMode.Monophonic;
        public string DefaultKeyboardSender { get; set; } = "SendInput";
        public Dictionary<string, KeyBinding> KeyMapping { get; set; } = new();
    }
}
