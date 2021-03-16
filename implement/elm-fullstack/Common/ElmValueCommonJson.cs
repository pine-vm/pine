namespace Kalmit.ElmValueCommonJson
{
    public class Result<ErrT, OkT>
    {
        public ErrT[] Err;

        public OkT[] Ok;
    }
}
