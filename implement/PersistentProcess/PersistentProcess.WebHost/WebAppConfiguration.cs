namespace Kalmit.PersistentProcess
{
    public class WebAppConfigurationJsonStructure
    {
        public ConditionalMapFromStringToString[] mapsFromRequestUrlToStaticFileName;

        public RateLimitWindow singleRateLimitWindowPerClientIPv4Address;

        public FluffySpoon.AspNet.LetsEncrypt.LetsEncryptOptions letsEncryptOptions;

        public string frontendWebElmMakeCommandAppendix;

        public class ConditionalMapFromStringToString
        {
            public string matchingRegexPattern;

            public string resultString;
        }

        public class RateLimitWindow
        {
            public int windowSizeInMs;

            public int limit;
        }
    }
}
