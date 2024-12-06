namespace DoveDraft.SymbolAliases;

[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property | AttributeTargets.Method)]
public sealed class AliasAttribute : Attribute
{
    public string AliasName { get; private set; }

    public AliasAttribute(string aliasName)
    {
        AliasName = aliasName;
    }
}