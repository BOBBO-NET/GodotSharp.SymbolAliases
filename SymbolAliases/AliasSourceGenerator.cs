using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace GodotSharp.SymbolAliases;

[Generator]
internal class AliasSourceGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // Define how to locate our attribute, and get what data we want
        var fieldDeclarations = context.SyntaxProvider.CreateSyntaxProvider(
            predicate: IsNodeOurSymbol,
            transform: GetSymbolInfo)
        .Where(fieldInfo => fieldInfo != null);
        
        // Combine all found field declarations into a single output
        var compilation = context.CompilationProvider.Combine(fieldDeclarations.Collect());

        context.RegisterSourceOutput(compilation, GenerateSource);
    }
    
    //
    //  Private Static Methods
    //
    
    private static bool IsNodeOurSymbol(SyntaxNode node, CancellationToken _)
    {
        // If this node is a property or field that has at least one attribute, return true
        return node switch
        {
            FieldDeclarationSyntax { AttributeLists.Count: > 0 } => true,
            PropertyDeclarationSyntax { AttributeLists.Count: > 0 } => true,
            MethodDeclarationSyntax { AttributeLists.Count: > 0 } => true,
            _ => false
        };
    }

    private static (INamedTypeSymbol ContainingType, ISymbol Symbol, string AliasName)? GetSymbolInfo(GeneratorSyntaxContext context, CancellationToken _) => context.Node switch
    {
        FieldDeclarationSyntax declarationSyntax => GetFieldInfo(declarationSyntax, context.SemanticModel, _),
        PropertyDeclarationSyntax declarationSyntax => GetPropertyInfo(declarationSyntax, context.SemanticModel, _),
        MethodDeclarationSyntax declarationSyntax => GetMethodInfo(declarationSyntax, context.SemanticModel, _),
        _ => null
    };
    
    private static (INamedTypeSymbol ContainingType, ISymbol Symbol, string AliasName)? GetFieldInfo(
        FieldDeclarationSyntax fieldDeclaration, SemanticModel semanticModel, CancellationToken _)
    {
        foreach (VariableDeclaratorSyntax variable in fieldDeclaration.Declaration.Variables)
        {
            // If this symbol isn't a compatible one, MOVE ON
            ISymbol symbol = semanticModel.GetDeclaredSymbol(variable, cancellationToken: _);
            if(symbol is not IFieldSymbol) continue;

            // If the field doesn't have our attribute, MOVE ON
            AttributeData aliasAttribute = symbol.GetAttributes()
                .FirstOrDefault(attr => attr.AttributeClass?.Name == nameof(AliasAttribute));
            if (aliasAttribute == null) continue;

            // OTHERWISE - we are compiling something relevant!
            var aliasName = aliasAttribute.ConstructorArguments[0].Value?.ToString();
            return (symbol.ContainingType, symbol, aliasName);
        }

        return null;
    }
    
    private static (INamedTypeSymbol ContainingType, ISymbol Symbol, string AliasName)? GetPropertyInfo(
        PropertyDeclarationSyntax propertyDeclaration, SemanticModel semanticModel, CancellationToken _)
    {
        // If this symbol isn't a compatible one, EXIT EARLY
        ISymbol symbol = semanticModel.GetDeclaredSymbol(propertyDeclaration, cancellationToken: _);
        if (symbol is not IPropertySymbol) return null;
        
        // If the property doesn't have our attribute, MOVE ON
        AttributeData aliasAttribute = symbol.GetAttributes()
            .FirstOrDefault(attr => attr.AttributeClass?.Name == nameof(AliasAttribute));
        if (aliasAttribute == null) return null;

        // OTHERWISE - we are compiling something relevant!
        var aliasName = aliasAttribute.ConstructorArguments[0].Value?.ToString();
        return (symbol.ContainingType, symbol, aliasName);
    }
    
    private static (INamedTypeSymbol ContainingType, ISymbol Symbol, string AliasName)? GetMethodInfo(
        MethodDeclarationSyntax methodDeclaration, SemanticModel semanticModel, CancellationToken _)
    {
        // If this symbol isn't a compatible one, EXIT EARLY
        ISymbol symbol = semanticModel.GetDeclaredSymbol(methodDeclaration, cancellationToken: _);
        if (symbol is not IMethodSymbol methodSymbol) return null;

        // If the method doesn't have our attribute, MOVE ON
        AttributeData aliasAttribute = symbol.GetAttributes()
            .FirstOrDefault(attr => attr.AttributeClass?.Name == nameof(AliasAttribute));
        if (aliasAttribute == null) return null;

        // OTHERWISE - we are compiling something relevant!
        var aliasName = aliasAttribute.ConstructorArguments[0].Value?.ToString();
        return (methodSymbol.ContainingType, methodSymbol, aliasName);
    }
    
    private static void GenerateSource(SourceProductionContext context, (Compilation Left, ImmutableArray<(INamedTypeSymbol ContainingType, ISymbol Symbol, string AliasName)?> Right) source)
    {
        (Compilation _, var fieldInfos) = source;
        
        foreach (var fieldInfo in fieldInfos)
        {
            if (fieldInfo is { ContainingType: { } containingType, Symbol: var symbol, AliasName: var aliasName })
            {
                switch (symbol)
                {
                    case IFieldSymbol fieldSymbol:
                        GenerateSourceField(context, containingType, fieldSymbol, aliasName);
                        break;
                    case IPropertySymbol propertySymbol:
                        GenerateSourceProperty(context, containingType, propertySymbol, aliasName);
                        break;
                    case IMethodSymbol methodSymbol:
                        GenerateSourceMethod(context, containingType, methodSymbol, aliasName);
                        break;
                }
            }
        }
    }

    private static void GenerateSourceField(SourceProductionContext context, INamedTypeSymbol containingType, IFieldSymbol symbol, string aliasName)
    {
        var namespaceName = containingType.ContainingNamespace.IsGlobalNamespace
            ? string.Empty
            : containingType.ContainingNamespace.ToDisplayString();
        
        AddAliasSource(
            context,
            namespaceName,
            containingType.Name,
            symbol.Type.ToDisplayString(),
            aliasName,
            symbol.Name,
            symbol.DeclaredAccessibility,
            hasGetter: true, // Fields always have a getter
            hasSetter: !symbol.IsReadOnly); // Fields have a setter unless they are readonly
    }

    private static void GenerateSourceProperty(SourceProductionContext context, INamedTypeSymbol containingType, IPropertySymbol symbol, string aliasName)
    {
        string namespaceName = containingType.ContainingNamespace.IsGlobalNamespace
            ? string.Empty
            : containingType.ContainingNamespace.ToDisplayString();
        
        AddAliasSource(
            context,
            namespaceName,
            containingType.Name,
            symbol.Type.ToDisplayString(),
            aliasName,
            symbol.Name,
            symbol.DeclaredAccessibility,
            hasGetter: symbol.GetMethod != null,
            hasSetter: symbol.SetMethod != null,
            setterAccessibility: symbol.SetMethod?.DeclaredAccessibility);
    }

    private static void AddAliasSource(SourceProductionContext context, string namespaceName, string className, string memberType, string aliasName, string originalMemberName, Accessibility getterAccessibility, bool hasGetter, bool hasSetter, Accessibility? setterAccessibility = null)
    {
        var sourceCodeBuilder = new StringBuilder();
        string getterAccessModifier = GetterAccessibilityString(getterAccessibility);
        string setterAccessModifier = SetterAccessibilityString(setterAccessibility);
        
        // Open the namespace
        if (!string.IsNullOrWhiteSpace(namespaceName))
        {
            sourceCodeBuilder.AppendLine($"namespace {namespaceName}");
            sourceCodeBuilder.AppendLine("{");
        }

        // Open the class
        sourceCodeBuilder.AppendLine($"partial class {className}");
        sourceCodeBuilder.AppendLine("{");
        
        // Open the property
        sourceCodeBuilder.AppendLine($"{getterAccessModifier} {memberType} {aliasName}");
        sourceCodeBuilder.AppendLine("{");
        
        // Add the getter
        if (hasGetter)
        {
            sourceCodeBuilder.AppendLine($"get => this.{originalMemberName};");
        }
        
        // Add the setter
        if (hasSetter)
        {
            sourceCodeBuilder.AppendLine($"{setterAccessModifier} set => this.{originalMemberName} = value;");
        }
        
        // Close the property
        sourceCodeBuilder.AppendLine("}");
        
        // Close the class
        sourceCodeBuilder.AppendLine("}");
        
        // Close the namespace
        if (!string.IsNullOrWhiteSpace(namespaceName))
        {
            sourceCodeBuilder.AppendLine("}");
        }
        
        context.AddSource($"{className}_{originalMemberName}_{aliasName}_Alias.g.cs", SourceText.From(sourceCodeBuilder.ToString(), Encoding.UTF8));
    }
    
    private static void GenerateSourceMethod(SourceProductionContext context, INamedTypeSymbol containingType, IMethodSymbol symbol, string aliasName)
    {
        var sourceCodeBuilder = new StringBuilder();
        var namespaceName = containingType.ContainingNamespace.IsGlobalNamespace
            ? string.Empty
            : containingType.ContainingNamespace.ToDisplayString();
        string accessibility = MethodAccessibilityString(symbol.DeclaredAccessibility);
        string returnType = symbol.ReturnType.ToDisplayString();
        string parameters = string.Join(", ", symbol.Parameters.Select(p => $"{p.Type.ToDisplayString()} {p.Name}{(p.HasExplicitDefaultValue ? " = " + p.ExplicitDefaultValue : "")}"));
        string parameterNames = string.Join(", ", symbol.Parameters.Select(p => p.Name));
    
        // Open the namespace
        if (!string.IsNullOrWhiteSpace(namespaceName))
        {
            sourceCodeBuilder.AppendLine($"namespace {namespaceName}");
            sourceCodeBuilder.AppendLine("{");
        }
    
        // Open the class
        sourceCodeBuilder.AppendLine($"partial class {containingType.Name}");
        sourceCodeBuilder.AppendLine("{");
    
        // Open the method
        sourceCodeBuilder.AppendLine($"{accessibility} {returnType} {aliasName}({parameters})");
        sourceCodeBuilder.AppendLine("{");
        
        // Add the method body
        if (returnType != "void")
        {
            sourceCodeBuilder.AppendLine($"return this.{symbol.Name}({parameterNames});");
        }
        else
        {
            sourceCodeBuilder.AppendLine($"this.{symbol.Name}({parameterNames});");
        }
        
        // Close the method
        sourceCodeBuilder.AppendLine("}");
    
        // Close the class
        sourceCodeBuilder.AppendLine("}");

        // Close the namespace
        if (!string.IsNullOrWhiteSpace(namespaceName))
        {
            sourceCodeBuilder.AppendLine("}");
        }
    
        context.AddSource($"{containingType.Name}_{symbol.Name}_{aliasName}_Alias.g.cs", SourceText.From(sourceCodeBuilder.ToString(), Encoding.UTF8));
    }
    
    private static string GetterAccessibilityString(Accessibility accessibility) => accessibility switch
    {
        Accessibility.Public => "public",
        Accessibility.Internal => "internal",
        Accessibility.Protected => "protected",
        Accessibility.Private => "private",
        _ => "private" // Default to private if unclear
    };
    
    private static string SetterAccessibilityString(Accessibility? accessibility) => accessibility switch
    {
        Accessibility.Internal => "internal ",
        Accessibility.Protected => "protected ",
        Accessibility.Private => "private ",
        _ => "" // Default is the same as the property
    };
    
    private static string MethodAccessibilityString(Accessibility accessibility) => accessibility switch
    {
        Accessibility.Public => "public",
        Accessibility.Internal => "internal",
        Accessibility.Protected => "protected",
        Accessibility.Private => "private",
        _ => "private" // Default to private if unclear
    };
}