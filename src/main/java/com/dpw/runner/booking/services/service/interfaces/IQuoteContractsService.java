package com.dpw.runner.booking.services.service.interfaces;

import com.dpw.runner.booking.services.dto.response.ListContractResponse;

public interface IQuoteContractsService extends ICommonService{

    void updateQuoteContracts(ListContractResponse request);
}
