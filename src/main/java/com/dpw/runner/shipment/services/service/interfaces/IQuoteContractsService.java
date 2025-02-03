package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.entity.QuoteContracts;

public interface IQuoteContractsService extends ICommonService{

    void updateQuoteContracts(ListContractResponse request);

    QuoteContracts getQuoteContractsByContractId(String contractId);
}
