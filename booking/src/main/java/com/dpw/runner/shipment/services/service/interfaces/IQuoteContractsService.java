package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.response.ListContractResponse;

public interface IQuoteContractsService extends ICommonService{

    void updateQuoteContracts(ListContractResponse request);
}
