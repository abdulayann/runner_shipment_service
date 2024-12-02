package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.dto.request.ConsoleBookingRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public interface IConsolidationDetailsDao {
    ConsolidationDetails save(ConsolidationDetails consolidationDetails, boolean fromV1Sync);
    ConsolidationDetails save(ConsolidationDetails consolidationDetails, boolean fromV1Sync, boolean creatingFromDgShipment);
    ConsolidationDetails update(ConsolidationDetails consolidationDetails, boolean fromV1Sync);
    ConsolidationDetails update(ConsolidationDetails consolidationDetails, boolean fromV1Sync, boolean updatingFromDgShipment);
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
    List<ConsolidationDetails> findConsolidationsByGuids(Set<UUID> guids);
    List<ConsolidationDetails> findConsolidationsByIds(Set<Long> ids);
    ConsolidationDetails findConsolidationsById(Long id);
    List<ConsolidationDetailsProjection> findMblNumberInDifferentTenant(String mblNumber);
    Page<Long> getIdWithPendingActions(ShipmentRequestedType shipmentRequestedType, Pageable pageable);
    List<ConsolidationDetails> findBySourceGuid(UUID guid);
    void entityDetach(List<ConsolidationDetails> consolidationDetails);
    Optional<ConsolidationDetails> findConsolidationByIdWithQuery(Long id);
    void saveIsTransferredToReceivingBranch(Long id, Boolean entityTransferred);
}
