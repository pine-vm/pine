using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;

namespace Pine.UnitTests;

[TestClass]
public class ReusedInstancesTests
{
    [TestMethod]
    public void Ensure_reference_equality_between_mappings_between_reused_instances()
    {
        ReusedInstances.Instance.AssertReferenceEquality();
    }
}
