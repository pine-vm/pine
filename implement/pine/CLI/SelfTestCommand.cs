using System.CommandLine;

using Test = ElmTime.Test;

namespace Pine.CLI;

public static class SelfTestCommand
{
    public static Command Create()
    {
        var command = new Command("self-test", "Tests integration of native dependencies");

        command.SetAction(
            (parseResult) =>
            {
                return Test.SelfTest.RunAllTestsAndPrintToConsole();
            });

        return command;
    }
}
