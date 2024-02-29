package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.npm.UpdateContractRequest;
import com.dpw.runner.shipment.services.dto.response.npm.NPMFetchLangChargeCodeResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;
import org.testcontainers.shaded.org.bouncycastle.jcajce.provider.asymmetric.ec.KeyFactorySpi;

public interface INPMServiceAdapter {
    ResponseEntity<IRunnerResponse> fetchContracts(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> fetchContractsTemp(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> fetchContract(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> updateContracts(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> fetchOffers(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> fetchOffersV8(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> awbAutoSell(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> awbImportRates(CommonRequestModel commonRequestModel) throws RunnerException;
    NPMFetchLangChargeCodeResponse fetchMultiLangChargeCode(CommonRequestModel commonRequestModel) throws RunnerException;
}
