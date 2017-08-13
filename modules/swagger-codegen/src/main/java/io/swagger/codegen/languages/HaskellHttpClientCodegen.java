package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.util.*;
import java.util.regex.Pattern;

import org.apache.commons.io.FileUtils;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.SupportingFile;
import io.swagger.util.Json;
import java.io.IOException;
import java.io.File;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;

public class HaskellHttpClientCodegen extends DefaultCodegen implements CodegenConfig {

    // source folder where to write the files
    protected String sourceFolder = "src";
    protected String apiVersion = "0.0.1";
    protected String GENERATE_LENSES = "generateLenses";
    protected String DERIVING = "deriving";
    protected String NO_JSON_NULLS = "noJsonNulls";
//    protected String MODEL_IMPORTS = "modelImports";
//    protected String MODEL_EXTENSIONS = "modelExtensions";
    private static final Pattern LEADING_UNDERSCORE = Pattern.compile("^_+");

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -l flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "haskell-http-client";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a Haskell http-client library.";
    }

    public HaskellHttpClientCodegen() {
        super();

        // override the mapping to keep the original mapping in Haskell
        specialCharReplacements.put("-", "Dash");
        specialCharReplacements.put(">", "GreaterThan");
        specialCharReplacements.put("<", "LessThan");

        // backslash and double quote need double the escapement for both Java and Haskell
        specialCharReplacements.remove("\\");
        specialCharReplacements.remove("\"");
        specialCharReplacements.put("\\\\", "Back_Slash");
        specialCharReplacements.put("\\\"", "Double_Quote");

        // set the output folder here
        outputFolder = "generated-code/haskell-http-client";

    /*
     * Template Location.  This is the location which templates will be read from.  The generator
     * will use the resource stream to attempt to read the templates.
     */
        embeddedTemplateDir = templateDir = "haskell-http-client";

    /*
     * Api Package.  Optional, if needed, this can be used in templates
     */
        apiPackage = "API";

    /*
     * Model Package.  Optional, if needed, this can be used in templates
     */
        modelPackage = "Types";

        // Haskell keywords and reserved function names, taken mostly from https://wiki.haskell.org/Keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        // Keywords
                        "as", "case", "of",
                        "class", "data", "family",
                        "default", "deriving",
                        "do", "forall", "foreign", "hiding",
                        "if", "then", "else",
                        "import", "infix", "infixl", "infixr",
                        "instance", "let", "in",
                        "mdo", "module", "newtype",
                        "proc", "qualified", "rec",
                        "type", "where"
                )
        );

    /*
     * Additional Properties.  These values can be passed to the templates and
     * are available in models, apis, and supporting files
     */
        additionalProperties.put("apiVersion", apiVersion);

    /*
     * Supporting Files.  You can write single files for the generator with the
     * entire object tree available.  If the input file has a suffix of `.mustache
     * it will be processed by the template engine.  Otherwise, it will be copied
     */
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("stack.mustache", "", "stack.yaml"));
        supportingFiles.add(new SupportingFile("Setup.mustache", "", "Setup.hs"));
        supportingFiles.add(new SupportingFile(".gitignore", "", ".gitignore"));

    /*
     * Supporting Test Files.
    */
        supportingFiles.add(new SupportingFile("tests/ApproxEq.mustache", "tests", "ApproxEq.hs"));
        supportingFiles.add(new SupportingFile("tests/Instances.mustache", "tests", "Instances.hs"));
        supportingFiles.add(new SupportingFile("tests/PropJSON.mustache", "tests", "PropJSON.hs"));
        supportingFiles.add(new SupportingFile("tests/Test.mustache", "tests", "Test.hs"));
    /*
     * Language Specific Primitives.  These types will not trigger imports by
     * the client generator
     */
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "Bool",
                        "String",
                        "Int",
                        "Integer",
                        "Float",
                        "Char",
                        "Double",
                        "List",
                        "FilePath"
                )
        );

        typeMapping.clear();
        typeMapping.put("array", "List");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "Text");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Integer");
        typeMapping.put("short", "Int");
        typeMapping.put("char", "Char");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("DateTime", "Integer");
        typeMapping.put("file", "FilePath");
        typeMapping.put("number", "Double");
        typeMapping.put("integer", "Int");
        typeMapping.put("any", "Value");
        typeMapping.put("UUID", "Text");

        importMapping.clear();
        importMapping.put("Map", "qualified Data.Map as Map");

        //cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        //cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));

        cliOptions.add(new CliOption(NO_JSON_NULLS, "fail when encountering JSON Null during model decoding"));
        cliOptions.add(new CliOption(GENERATE_LENSES, "Generate Lens optics for Models"));
        cliOptions.add(new CliOption(DERIVING, "Additional classes to include in the deriving() clause of Models"));
//        cliOptions.add(new CliOption(MODEL_IMPORTS, "Additional imports in the Models file"));
//        cliOptions.add(new CliOption(MODEL_EXTENSIONS, "Additional extensions in the Models file"));
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reseved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    public String firstLetterToUpper(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toUpperCase();
        } else {
            return word.substring(0, 1).toUpperCase() + word.substring(1);
        }
    }

    public String firstLetterToLower(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toLowerCase();
        } else {
            return word.substring(0, 1).toLowerCase() + word.substring(1);
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(DERIVING)) {
            String deriving = (String)additionalProperties.get(DERIVING);
            additionalProperties.put(DERIVING, StringUtils.join(deriving.split(" "),","));
        }
    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        // From the title, compute a reasonable name for the package and the API
        String title = swagger.getInfo().getTitle();

        // Drop any API suffix
        if(title == null) {
            title = "Swagger";
        } else {
            title = title.trim();
            if (title.toUpperCase().endsWith("API")) {
                title = title.substring(0, title.length() - 3);
            }
        }

        String[] words = title.split(" ");

        // The package name is made by appending the lowercased words of the title interspersed with dashes
        List<String> wordsLower = new ArrayList<String>();
        for (String word : words) {
            wordsLower.add(word.toLowerCase());
        }
        String cabalName = StringUtils.join(wordsLower,"-");
        String pathsName = StringUtils.join(wordsLower,"_");

        // The API name is made by appending the capitalized words of the title
        List<String> wordsCaps = new ArrayList<String>();
        for (String word : words) {
            wordsCaps.add(firstLetterToUpper(word));
        }
        String apiName = StringUtils.join(wordsCaps,"");

        // Set the filenames to write for the API

        // root
        supportingFiles.add(new SupportingFile("haskell-http-client.cabal.mustache", "", cabalName + ".cabal"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.yaml"));

        // lib
        supportingFiles.add(new SupportingFile("TopLevel.mustache", "lib/", apiName + ".hs"));
        supportingFiles.add(new SupportingFile("API.mustache", "lib/" + apiName, "API.hs"));
        supportingFiles.add(new SupportingFile("Client.mustache", "lib/" + apiName, "Client.hs"));
        supportingFiles.add(new SupportingFile("Model.mustache", "lib/" + apiName, "Model.hs"));

        // lens
        if (additionalProperties.containsKey(GENERATE_LENSES)) {
            supportingFiles.add(new SupportingFile("Lens.mustache", "lib/" + apiName, "Lens.hs"));
        }

        additionalProperties.put("title", apiName);
        additionalProperties.put("titleLower", firstLetterToLower(apiName));
        additionalProperties.put("package", cabalName);
        additionalProperties.put("pathsName", pathsName);
        additionalProperties.put("requestType", apiName + "Request");
        additionalProperties.put("configType", apiName + "Config");
        additionalProperties.put("swaggerVersion", swagger.getSwagger());

        List<Map<String, Object>> replacements = new ArrayList<>();
        Object[] replacementChars = specialCharReplacements.keySet().toArray();
        for(int i = 0; i < replacementChars.length; i++) {
            String c = (String) replacementChars[i];
            Map<String, Object> o = new HashMap<>();
            o.put("char", c);
            o.put("replacement", "'" + specialCharReplacements.get(c));
            o.put("hasMore", i != replacementChars.length - 1);
            replacements.add(o);
        }
        additionalProperties.put("specialCharReplacements", replacements);

        //copy input swagger to output folder
        try {
            String swaggerJson = Json.pretty(swagger);
            FileUtils.writeStringToFile(new File(outputFolder + File.separator + "swagger.json"), swaggerJson);
        } catch (IOException e) {
            throw new RuntimeException(e.getMessage(), e.getCause());
        }

        super.preprocessSwagger(swagger);
    }


    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "Map.Map String " + getTypeDeclaration(inner);
        }
        return fixModelChars(super.getTypeDeclaration(p));
    }

    /**
     * Optional - swagger type conversion.  This is used to map swagger types in a `Property` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     * @see Property
     */
    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type))
                return toModelName(type);
        } else if(swaggerType == "object") {
            type = "Value";
        } else if(typeMapping.containsValue(swaggerType)) {
            type = swaggerType + "_";
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            Property additionalProperties2 = ap.getAdditionalProperties();
            String type = additionalProperties2.getType();
            if (null == type) {
                LOGGER.error("No Type defined for Additional Property " + additionalProperties2 + "\n" //
                        + "\tIn Property: " + p);
            }
            String inner = getSwaggerType(additionalProperties2);
            return "(Map.Map Text " + inner + ")";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            // Return only the inner type; the wrapping with QueryList is done
            // somewhere else, where we have access to the collection format.
            return inner;
        } else {
            return null;
        }
    }

    @Override
    public CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = super.fromOperation(resourcePath, httpMethod, operation, definitions, swagger);

        op.vendorExtensions.put("x-baseOperationId", op.operationId);
        op.vendorExtensions.put("x-haddockPath", String.format("%s %s", op.httpMethod, op.path.replace("/","\\/")));
        op.operationId = toHsVarName(op.operationId);
        op.vendorExtensions.put("x-operationType", capitalize(op.operationId));

        for (CodegenParameter param : op.allParams) {
           if(!param.required)  {
               op.vendorExtensions.put("x-hasOptionalParams", true);
               param.vendorExtensions.put("x-paramNameType", capitalize(param.paramName));
               param.vendorExtensions.put("x-operationType", capitalize(op.operationId));
           }
        }
        for (CodegenParameter param : op.bodyParams) { }
        if(op.getHasPathParams()) {
            String remainingPath = op.path;
            for (CodegenParameter param : op.pathParams) {
                param.paramName = toHsVarName(param.paramName);
                String[] pieces = remainingPath.split("\\{"+param.baseName+"\\}");
                if(pieces.length == 0) throw new RuntimeException("paramName {"+param.baseName+"} not in path " + op.path);
                if(pieces.length > 2) throw new RuntimeException("paramName {"+param.baseName+"} found multiple times in path " + op.path);
                if(pieces.length == 2) {
                    param.vendorExtensions.put("x-pathPrefix", pieces[0]);
                    remainingPath = pieces[1];
                } else {
                    if(remainingPath.startsWith("{"+param.baseName+"}")) {
                        remainingPath = pieces[0];
                    }
                    else {
                        param.vendorExtensions.put("x-pathPrefix", pieces[0]);
                        remainingPath = "";
                    }
                }
            }
            op.vendorExtensions.put("x-hasPathParams", true);
            if(remainingPath.length() > 0) { op.vendorExtensions.put("x-pathSuffix", remainingPath); }
        } else {
            op.vendorExtensions.put("x-hasPathParams", false);
            op.vendorExtensions.put("x-pathSuffix", op.path);
        }
        for (CodegenParameter param : op.queryParams) { }
        for (CodegenParameter param : op.headerParams) { }
        for (CodegenParameter param : op.formParams) { }

        String returnType = op.returnType;
        if (returnType == null || returnType.equals("null")) {
            returnType = "()";
        }
        if (returnType.indexOf(" ") >= 0) {
            returnType = "(" + returnType + ")";
        }
        op.vendorExtensions.put("x-returnType", returnType);

        return op;
    }

    private String toHsVarName(String paramName) {
        return toVarName(camelize(fixOperatorChars(fixModelChars(paramName)), true));
    }

    private String fixOperatorChars(String string) {
        StringBuilder sb = new StringBuilder();
        String name = string;
        //Check if it is a reserved word, in which case the underscore is added when property name is generated.
        if (string.startsWith("_")) {
            if (reservedWords.contains(string.substring(1, string.length()))) {
                name = string.substring(1, string.length());
            } else if (reservedWordsMappings.containsValue(string)) {
                name = LEADING_UNDERSCORE.matcher(string).replaceFirst("");
            }
        }
        for (char c : name.toCharArray()) {
            String cString = String.valueOf(c);
            if (specialCharReplacements.containsKey(cString)) {
                sb.append("'");
                sb.append(specialCharReplacements.get(cString));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    // Remove characters from a string that do not belong in a model classname
    private String fixModelChars(String string) {
        return string.replace(".", "").replace("-", "");
    }

    private String capitalize(String word) {
        if (word.length() > 0) {
            word = word.substring(0, 1).toUpperCase() + word.substring(1);
        }

        return word;
    }
    // Override fromModel to create the appropriate model namings
    @Override
    public CodegenModel fromModel(String name, Model mod, Map<String, Model> allDefinitions) {
        CodegenModel model = super.fromModel(name, mod, allDefinitions);

        // Clean up the class name to remove invalid characters
        model.classname = fixModelChars(model.classname);
        if(typeMapping.containsValue(model.classname)) {
            model.classname += "_";
        }

        // From the model name, compute the prefix for the fields.
        String prefix = camelize(model.classname, true);
        for(CodegenProperty prop : model.vars) {
            prop.name = toVarName(prefix + camelize(fixOperatorChars(prop.name)));
        }

        //String dataOrNewtype = "data";
        // check if it's a ModelImpl before casting 
        if (!(mod instanceof ModelImpl)) {
            return model;
        }

          // Create newtypes for things with non-object types
//        String modelType = ((ModelImpl)  mod).getType();
//        if(modelType != "object" && typeMapping.containsKey(modelType)) {
//            String newtype = typeMapping.get(modelType);
//            model.vendorExtensions.put("x-customNewtype", newtype);
//        }

        return model;
    }

    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        CodegenParameter p = super.fromParameter(param, imports);
        p.paramName = toHsVarName(p.baseName);
        p.dataType = fixModelChars(p.dataType);
        return p;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("{-", "{_-").replace("-}", "-_}");
    }

}
