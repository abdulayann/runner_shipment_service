package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.npm.UpdateContractRequest;
import com.dpw.runner.shipment.services.dto.response.npm.NPMFetchLangChargeCodeResponse;
import org.springframework.http.ResponseEntity;
import org.testcontainers.shaded.org.bouncycastle.jcajce.provider.asymmetric.ec.KeyFactorySpi;

public interface INPMServiceAdapter {
    ResponseEntity<IRunnerResponse> fetchContracts(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<IRunnerResponse> fetchContractsTemp(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> fetchContract(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> updateContracts(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> fetchOffers(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> fetchOffersV8(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> awbAutoSell(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<IRunnerResponse> awbImportRates(CommonRequestModel commonRequestModel) throws Exception;
    NPMFetchLangChargeCodeResponse fetchMultiLangChargeCode(CommonRequestModel commonRequestModel) throws Exception;
}
