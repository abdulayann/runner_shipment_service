package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import org.springframework.http.ResponseEntity;

public interface ITransactionHistoryService {

    /**
     * Retrieve a TransactionHistory by entityID and entityType.
     *
     * @param entityId entityId
     * @param entityType entityType
     * @return ResponseEntity<IRunnerResponse>
     */
    ResponseEntity<IRunnerResponse> retrieveById(Long entityId, EntityTypeTransactionHistory entityType);

}
