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
    Optional<ReferenceNumbers> findById(Long id);
    void delete(ReferenceNumbers referenceNumbers);
    List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId) throws RunnerException;
    List<ReferenceNumbers> saveEntityFromShipment(List<ReferenceNumbers> referenceNumbersRequests, Long shipmentId);
    List<ReferenceNumbers> saveEntityFromShipment(List<ReferenceNumbers> referenceNumbersRequests, Long shipmentId, Map<Long, ReferenceNumbers> hashMap);
    List<ReferenceNumbers> updateEntityFromConsole(List<ReferenceNumbers> referenceNumbersList, Long consolidationId, Long carrierBookingId) throws RunnerException;
    List<ReferenceNumbers> updateEntityFromCarrierBooking(List<ReferenceNumbers> referenceNumbersList, Long carrierBookingId) throws RunnerException;
    List<ReferenceNumbers> updateEntityFromConsole(List<ReferenceNumbers> referenceNumbersList, Long consolidationId, List<ReferenceNumbers> oldEntityList) throws RunnerException;
    List<ReferenceNumbers> saveEntityFromConsole(List<ReferenceNumbers> referenceNumbersRequests, Long consolidationId);
    List<ReferenceNumbers> saveEntityFromCarrierBooking(List<ReferenceNumbers> referenceNumbersRequests, Long carrierBookingId);
    List<ReferenceNumbers> saveEntityFromCarrierBooking(List<ReferenceNumbers> referenceNumbersRequests, Long carrierBookingId, Map<Long, ReferenceNumbers> hashMap);
    List<ReferenceNumbers> saveEntityFromConsole(List<ReferenceNumbers> referenceNumbersRequests, Long consolidationId, Long carrierBookingId, Map<Long, ReferenceNumbers> hashMap);
    List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId, List<ReferenceNumbers> oldEntityList) throws RunnerException;
}
