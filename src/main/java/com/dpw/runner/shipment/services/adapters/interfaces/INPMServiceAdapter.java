package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.npm.UpdateContractRequest;
import org.springframework.http.ResponseEntity;
import org.testcontainers.shaded.org.bouncycastle.jcajce.provider.asymmetric.ec.KeyFactorySpi;

public interface INPMServiceAdapter {
    ResponseEntity<?> fetchContracts(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> fetchContractsTemp(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> fetchContract(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> updateContracts(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> fetchOffers(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> fetchOffersV8(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> awbAutoSell(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> awbImportRates(CommonRequestModel commonRequestModel) throws Exception;
}
