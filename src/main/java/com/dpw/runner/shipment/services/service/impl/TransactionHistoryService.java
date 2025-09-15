package com.dpw.runner.shipment.services.service.impl;

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
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

@Slf4j
@Service
public class TransactionHistoryService implements ITransactionHistoryService {

    private final IVerifiedGrossMassDao verifiedGrossMassDao;
    private final JsonHelper jsonHelper;
    private final CarrierBookingDao carrierBookingDao;
    private final IShippingInstructionDao shippingInstructionDao;
    private final CommonUtils commonUtils;
    private final ITransactionHistoryDao transactionHistoryDao;
    private final Map<EntityTypeTransactionHistory, Function<Long, String>> fetchDetailsBasisEntityType;


    public TransactionHistoryService(IVerifiedGrossMassDao verifiedGrossMassDao, JsonHelper jsonHelper,
                                     CarrierBookingDao carrierBookingDao, IShippingInstructionDao shippingInstructionDao, CommonUtils commonUtils,
                                    ITransactionHistoryDao transactionHistoryDao) {
        this.verifiedGrossMassDao = verifiedGrossMassDao;
        this.jsonHelper = jsonHelper;
        this.carrierBookingDao = carrierBookingDao;
        this.shippingInstructionDao = shippingInstructionDao;
        this.commonUtils = commonUtils;
        this.transactionHistoryDao = transactionHistoryDao;
        fetchDetailsBasisEntityType = new HashMap<>();
    }

    @PostConstruct
    void initStatusResolvers() {
        fetchDetailsBasisEntityType.put(EntityTypeTransactionHistory.VGM, this::resolveVgmStatus);
        fetchDetailsBasisEntityType.put(EntityTypeTransactionHistory.CARRIER_BOOKING, this::resolveCarrierBookingStatus);
        fetchDetailsBasisEntityType.put(EntityTypeTransactionHistory.SI, this::resolveShippingInstructionStatus);
    }

    private String resolveVgmStatus(Long entityId) {
        return verifiedGrossMassDao.findById(entityId)
                .map(vgm -> vgm.getStatus().getDescription())
                .orElseThrow(() -> new DataRetrievalFailureException("Verified Gross Mass not found for ID: " + entityId));
    }

    private String resolveCarrierBookingStatus(Long entityId) {
        return carrierBookingDao.findById(entityId)
                .map(cb -> cb.getStatus().getDescription())
                .orElseThrow(() -> new DataRetrievalFailureException("Carrier Booking not found for ID: " + entityId));
    }

    private String resolveShippingInstructionStatus(Long entityId) {
        return shippingInstructionDao.findById(entityId)
                .map(si -> si.getStatus().getDescription())
                .orElseThrow(() -> new DataRetrievalFailureException("Shipping Instruction not found for ID: " + entityId));
    }


    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(Long entityId, EntityTypeTransactionHistory entityType) {

        Function<Long, String> fetchTransactionHistoryDetails = fetchDetailsBasisEntityType.get(entityType);
        if (Objects.isNull(fetchTransactionHistoryDetails)) {
            throw new IllegalArgumentException("Unsupported entity type: " + entityType);
        }

        String actionStatusDescription = fetchTransactionHistoryDetails.apply(entityId);

        // Fetch transaction history records for the given VGM ID
        List<TransactionHistory> transactionHistoryList = transactionHistoryDao.findAllByEntityIdAndEntityType(entityId, entityType.name());

        // Return empty transaction if not found for the given VGM ID
        if (transactionHistoryList.isEmpty()) {
            return ResponseHelper.buildListSuccessResponse(new ArrayList<>());
        }

        // Map each TransactionHistory to a TransactionHistoryResponse and set the status description
        List<TransactionHistoryResponse> transactionHistoryResponseList = transactionHistoryList.stream()
                .map(transactionHistory -> {
                    TransactionHistoryResponse response = jsonHelper.convertValue(transactionHistory, TransactionHistoryResponse.class);
                    response.setActionStatusDescription(actionStatusDescription);
                    return response;
                })
                .toList();

        return ResponseHelper.buildListSuccessResponse(new ArrayList<>(transactionHistoryResponseList));
    }
}
