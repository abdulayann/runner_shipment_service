package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionStatus;

public interface IJobService extends ICommonService {
    ResponseEntity<?> createJob(CommonRequestModel commonRequestModel, TransactionStatus txStatus, PlatformTransactionManager transactionManager);
}
