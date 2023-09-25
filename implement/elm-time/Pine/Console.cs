using System.Collections.Generic;

namespace Pine;

public interface IConsole
{
    void WriteLine(string text) =>
        WriteLine(text, TextColor.Default);

    void WriteLine(string text, TextColor color) =>
        Write(text + "\n", color);

    void Write(string text, TextColor color) =>
        Write([(text, color)]);

    void Write(IReadOnlyList<(string text, TextColor color)> coloredTexts);

    public record struct ColoredText(string text, TextColor color);

    public enum TextColor
    {
        Default,
        Green,
        Red
    }
}

public class StaticConsole : IConsole
{
    public static readonly System.ConsoleColor InitialForegroundColor = System.Console.ForegroundColor;

    public static readonly StaticConsole Instance = new();

    public void Write(IReadOnlyList<(string text, IConsole.TextColor color)> coloredTexts)
    {
        foreach (var (text, color) in coloredTexts)
        {
            System.Console.ForegroundColor =
                color switch
                {
                    IConsole.TextColor.Default => InitialForegroundColor,
                    IConsole.TextColor.Green => System.ConsoleColor.Green,
                    IConsole.TextColor.Red => System.ConsoleColor.Red,
                    _ => throw new System.NotImplementedException()
                };

            System.Console.Write(text);
        }

        System.Console.ForegroundColor = InitialForegroundColor;
    }
}