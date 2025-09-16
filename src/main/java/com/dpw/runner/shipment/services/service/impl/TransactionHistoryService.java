package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.CarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.TransactionHistoryResponse;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ITransactionHistoryService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
public class TransactionHistoryService implements ITransactionHistoryService {

    private final IVerifiedGrossMassDao verifiedGrossMassDao;
    private final JsonHelper jsonHelper;
    private final CarrierBookingDao carrierBookingDao;
    private final IShippingInstructionDao shippingInstructionDao;
    private final ITransactionHistoryDao transactionHistoryDao;


    public TransactionHistoryService(IVerifiedGrossMassDao verifiedGrossMassDao, JsonHelper jsonHelper,
                                     CarrierBookingDao carrierBookingDao, IShippingInstructionDao shippingInstructionDao,
                                    ITransactionHistoryDao transactionHistoryDao) {
        this.verifiedGrossMassDao = verifiedGrossMassDao;
        this.jsonHelper = jsonHelper;
        this.carrierBookingDao = carrierBookingDao;
        this.shippingInstructionDao = shippingInstructionDao;
        this.transactionHistoryDao = transactionHistoryDao;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(Long entityId, EntityTypeTransactionHistory entityType) {

        validateIfEntityExists(entityType, entityId);

        // Fetch transaction history records for the given entity ID and type
        List<TransactionHistory> transactionHistoryList =
                transactionHistoryDao.findAllByEntityIdAndEntityType(entityId, entityType.name(), UserContext.getUser().getTenantId());

        // Return empty transaction if not found for the given entity ID and type
        if (transactionHistoryList.isEmpty()) {
            return ResponseHelper.buildListSuccessResponse(new ArrayList<>());
        }

        List<TransactionHistoryResponse> responseList = transactionHistoryList.stream()
                .map(transactionHistory ->
                        jsonHelper.convertValue(transactionHistory, TransactionHistoryResponse.class))
                .toList();

        return ResponseHelper.buildListSuccessResponse(new ArrayList<>(responseList));
    }

    private void validateIfEntityExists(EntityTypeTransactionHistory entityType, Long entityId) {
        Boolean isEntryExists;

        switch (entityType) {
            case VGM -> isEntryExists = verifiedGrossMassDao.existsById(entityId);
            case CARRIER_BOOKING -> isEntryExists = carrierBookingDao.existsById(entityId);
            case SI -> isEntryExists = shippingInstructionDao.existsById(entityId);
            default -> throw new IllegalArgumentException("Unsupported entity type: " + entityType);
        }

        if (!Boolean.TRUE.equals(isEntryExists)) {
            throw new DataRetrievalFailureException("No record found for " + entityType + " with ID: " + entityId);
        }
    }
}
