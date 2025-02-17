package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CommonErrorLogs;
import com.dpw.runner.shipment.services.entity.enums.CommonErrorType;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsoleValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentValidationResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ICommonErrorLogsDao {
    CommonErrorLogs save(CommonErrorLogs commonErrorLogs);

    Optional<CommonErrorLogs> findById(Long id);

    Optional<CommonErrorLogs> findByGuid(UUID guid);

    Page<CommonErrorLogs> findAll(Specification<CommonErrorLogs> spec, Pageable pageable);

    List<CommonErrorLogs> findByEntityIdAndEntityTypeAndErrorType(Long entityId, String entityType, CommonErrorType errorType);

    void logConsoleAutomaticTransferErrors(SendConsoleValidationResponse sendConsoleValidationResponse, Long consoleId, List<Long> shipmentIds);

    void deleteAllConsoleAndShipmentErrorsLogs(Long consoleId, List<Long> shipmentIds);

    void logShipmentAutomaticTransferErrors(SendShipmentValidationResponse sendShipmentValidationResponse, Long shipmentId);

    void deleteShipmentErrorsLogs(Long shipmentId);
}
