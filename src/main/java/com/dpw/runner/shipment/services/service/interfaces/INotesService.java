package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionStatus;

public interface INotesService extends ICommonService {

    ResponseEntity<?> createNote(CommonRequestModel commonRequestModel, TransactionStatus txStatus, PlatformTransactionManager transactionManager);
}
