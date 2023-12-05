package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IReferenceNumbersDao {
    ReferenceNumbers save(ReferenceNumbers referenceNumbers);
    Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable);
    Optional<ReferenceNumbers> findById(Long id);
    void delete(ReferenceNumbers referenceNumbers);
    List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId) throws Exception;
    List<ReferenceNumbers> saveEntityFromShipment(List<ReferenceNumbers> referenceNumbersRequests, Long shipmentId);
    List<ReferenceNumbers> updateEntityFromConsole(List<ReferenceNumbers> referenceNumbersList, Long consolidationId) throws Exception;
    List<ReferenceNumbers> updateEntityFromConsole(List<ReferenceNumbers> referenceNumbersList, Long consolidationId, List<ReferenceNumbers> oldEntityList) throws Exception;
    List<ReferenceNumbers> saveEntityFromConsole(List<ReferenceNumbers> referenceNumbersRequests, Long consolidationId);
    List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId, List<ReferenceNumbers> oldEntityList) throws Exception;
}
