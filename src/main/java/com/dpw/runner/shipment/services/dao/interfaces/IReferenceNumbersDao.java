package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface IReferenceNumbersDao {
    ReferenceNumbers save(ReferenceNumbers referenceNumbers);
    List<ReferenceNumbers> saveAll(List<ReferenceNumbers> referenceNumbersList);
    Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable);

    Page<ReferenceNumbers> findAllWithoutTenantFilter(Specification<ReferenceNumbers> spec, Pageable pageable);

    Optional<ReferenceNumbers> findById(Long id);
    void delete(ReferenceNumbers referenceNumbers);
    List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId) throws RunnerException;
    List<ReferenceNumbers> saveEntityFromShipment(List<ReferenceNumbers> referenceNumbersRequests, Long shipmentId);
    List<ReferenceNumbers>  saveEntityFromBooking(List<ReferenceNumbers> referenceNumbersRequests, Long bookingId);
    List<ReferenceNumbers>  updateEntityFromBooking(List<ReferenceNumbers> referenceNumbersRequests, Long bookingId) throws RunnerException;
    List<ReferenceNumbers> saveEntityFromShipment(List<ReferenceNumbers> referenceNumbersRequests, Long shipmentId, Map<Long, ReferenceNumbers> hashMap);
    List<ReferenceNumbers> updateEntityFromConsole(List<ReferenceNumbers> referenceNumbersList, Long consolidationId) throws RunnerException;
    List<ReferenceNumbers> updateEntityFromConsole(List<ReferenceNumbers> referenceNumbersList, Long consolidationId, List<ReferenceNumbers> oldEntityList) throws RunnerException;
    List<ReferenceNumbers> saveEntityFromConsole(List<ReferenceNumbers> referenceNumbersRequests, Long consolidationId);
    List<ReferenceNumbers> saveEntityFromConsole(List<ReferenceNumbers> referenceNumbersRequests, Long consolidationId, Map<Long, ReferenceNumbers> hashMap);
    List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId, List<ReferenceNumbers> oldEntityList) throws RunnerException;

    void deleteAdditionalDataByReferenceNumberIdsConsolidationId(List<Long> referenceNumberIds, Long consolidationId);

    void revertSoftDeleteByReferenceNumberIdsAndConsolidationId(List<Long> referenceNumberIds, Long consolidationId);

    void deleteAdditionalDataByReferenceNumberIdsBookingId(List<Long> referenceNumberIds, Long bookingId);

    void revertSoftDeleteByReferenceNumberIdsAndBookingId(List<Long> referenceNumberIds, Long bookingId);

    void deleteAdditionalreferenceNumbersByShipmentId(List<Long> referenceNumbersIds, Long shipmentId);

    void revertSoftDeleteByreferenceNumbersIdsAndShipmentId(List<Long> referenceNumbersIds, Long shipmentId);
}
