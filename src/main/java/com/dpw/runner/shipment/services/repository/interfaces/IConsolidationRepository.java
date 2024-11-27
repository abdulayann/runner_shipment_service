package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InterBranchEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;


@Repository @Generated @InterBranchEntity
public interface IConsolidationRepository extends MultiTenancyRepository<ConsolidationDetails> {
    List<ConsolidationDetails> findAll();
    Page<ConsolidationDetails> findAll(Specification<ConsolidationDetails> spec, Pageable pageable);
    default Optional<ConsolidationDetails> findById(Long id) {
        Specification<ConsolidationDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    default Optional<ConsolidationDetails> findByGuid (UUID guid) {
        Specification<ConsolidationDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), guid);
        return findOne(spec);
    }
    @Query(value = "SELECT * FROM consolidation_details WHERE bol = ?1 and tenant_id = ?2", nativeQuery = true)
    List<ConsolidationDetails> findByBol (String bol, Integer tenantId);

    @Query(value = "SELECT * FROM consolidation_details WHERE reference_number = ?1 and tenant_id = ?2", nativeQuery = true)
    List<ConsolidationDetails> findByReferenceNumber(String ref, Integer tenantId);

    @Query(value = "SELECT MAX(c.id) FROM consolidation_details c", nativeQuery = true)
    Long findMaxId();

    @Modifying
    @Query(value = "Update consolidation_details set booking_id = ?2, booking_status = ?3, booking_number = ?4 Where guid = ?1", nativeQuery = true)
    int updateConsoleBookingFields(UUID guid, String bookingId, String bookingStatus, String bookingNumber);

    @Modifying @Transactional
    @Query(value = "Update consolidation_details set created_by = ?2, created_at = ?3 Where id = ?1", nativeQuery = true)
    void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate);

    @Query(value = "SELECT consolidation_number FROM consolidation_details WHERE id = ?1", nativeQuery = true)
    String getConsolidationNumberFromId(Long id);

    @Query(value = "SELECT * FROM consolidation_details WHERE guid IN ?1", nativeQuery = true)
    List<ConsolidationDetails> findConsolidationsByGuids(Set<UUID> guids);

    @Query(value = "SELECT * FROM consolidation_details WHERE id IN ?1", nativeQuery = true)
    List<ConsolidationDetails> findConsolidationsByIds(Set<Long> ids);

    @Query(value = "SELECT * FROM consolidation_details WHERE id = ?1", nativeQuery = true)
    ConsolidationDetails getConsolidationFromId(Long id);

    @Query(value = "SELECT "
            + " tenant_id as tenantId, "
            + " consolidation_number as consolidationNumber, "
            + " mawb, "
            + " bol "
            + " FROM consolidation_details WHERE bol = ?1 AND tenant_id != ?2", nativeQuery = true)
    List<ConsolidationDetailsProjection> findMblNumberInDifferentTenant(String mblNumber, Integer tenantId);

    List<ConsolidationDetails> findBySourceGuid(UUID guid);

    @Query(value = "SELECT distinct c.id from ConsolidationDetails c inner join ConsoleShipmentMapping csm " +
        "on c.id = csm.consolidationId " +
        "where csm.isAttachmentDone = false and csm.requestedType = ?1")
    Page<Long> getIdWithPendingActions(ShipmentRequestedType shipmentRequestedType, Pageable pageable);

    @Query(value = "SELECT * FROM consolidation_details WHERE id = ?1", nativeQuery = true)
    Optional<ConsolidationDetails> findConsolidationByIdWithQuery(Long id);

    @Query(value = "SELECT id FROM consolidation_details WHERE guid = ?1", nativeQuery = true)
    Long findIdByGuid (UUID guid);

}
