package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.response.carrierbooking.TransactionHistoryResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import java.util.List;

public interface ITransactionHistoryService {

    /**
     * Retrieve a TransactionHistory by entityID and entityType.
     *
     * @param entityId entityId
     * @param entityType entityType
     * @return List<TransactionHistoryResponse>
     */
    List<TransactionHistoryResponse> retrieveById(Long entityId, EntityTypeTransactionHistory entityType);

}
