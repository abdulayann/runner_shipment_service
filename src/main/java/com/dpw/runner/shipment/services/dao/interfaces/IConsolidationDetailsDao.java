package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.dto.request.ConsoleBookingRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IConsolidationDetailsDao {
    ConsolidationDetails save(ConsolidationDetails consolidationDetails, boolean fromV1Sync);
    ConsolidationDetails update(ConsolidationDetails consolidationDetails, boolean fromV1Sync);
    Page<ConsolidationDetails> findAll(Specification<ConsolidationDetails> spec, Pageable pageable);
    Optional<ConsolidationDetails> findById(Long id);
    void delete(ConsolidationDetails consolidationDetails);
    List<ConsolidationDetails> saveAll(List<ConsolidationDetails> consolidationDetails);
    Optional<ConsolidationDetails> findByGuid (UUID guid);
    List<ConsolidationDetails> findByBol (String bol);
    List<ConsolidationDetails> findByReferenceNumber(String ref);
    Long findMaxId();
    Boolean isMAWBNumberValid(String masterBill);
    int updateConsoleBookingFields(ConsoleBookingRequest request);
    void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate);
    String getConsolidationNumberFromId(Long id);
}
