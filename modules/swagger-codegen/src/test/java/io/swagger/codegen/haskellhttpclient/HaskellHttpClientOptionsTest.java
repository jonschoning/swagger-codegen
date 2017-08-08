package io.swagger.codegen.haskellhttpclient;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.HaskellHttpClientCodegen;
import io.swagger.codegen.options.HaskellHttpClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class HaskellHttpClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private HaskellHttpClientCodegen clientCodegen;

    public HaskellHttpClientOptionsTest() {
        super(new HaskellHttpClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(HaskellHttpClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(HaskellHttpClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(HaskellHttpClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
