package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.response.IConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.entity.response.IShipmentResponse;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.InterBranchEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
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

    String listQuery = "WITH listQuery AS (SELECT consol.consolidationNumber, consol.consolidationType, consol.transportMode, consol.shipmentType, consol.isDomestic, consol.createdBy, consol.payment, consol.bookingCutoff, consol.estimatedTerminalCutoff, consol.terminalCutoff, consol.shipInstructionCutoff, consol.hazardousBookingCutoff, consol.verifiedGrossMassCutoff,"
        + "consol.reeferCutoff, consol.referenceNumber, consol.bookingStatus, consol.bookingNumber, consol.mawb, carrier.eta,carrier.ata,carrier.etd,carrier.atd,carrier.voyage,carrier.shippingLine,carrier.unlocationData, shipment.shipmentId, shipment.houseBill  FROM consolidation_details consol "
        + "inner join carrier_details carrier on consol.carrier_detail_id = carrier.id"
        + "inner join consol.shipmentsList shipment)";

    @Query(value = "SELECT consol.id as id,consol.createdBy as createdBy, consol.consolidationNumber as consolidationNumber, consol.consolidationType as consolidationType, consol.transportMode as transportMode, consol.shipmentType as shipmentType, consol.isDomestic as isDomestic, consol.createdBy as createdBy, consol.payment as payment, consol.bookingCutoff as bookingCutoff, consol.estimatedTerminalCutoff as estimatedTerminalCutoff, consol.terminalCutoff as terminalCutoff, consol.shipInstructionCutoff as shipInstructionCutoff, consol.hazardousBookingCutoff as hazardousBookingCutoff, consol.verifiedGrossMassCutoff as verifiedGrossMassCutoff,"
        + "consol.reeferCutoff as reeferCutoff, consol.referenceNumber as referenceNumber, consol.bookingStatus as bookingStatus, consol.bookingNumber as bookingNumber, consol.mawb as mawb, carrier.eta as eta,carrier.ata as ata,carrier.etd as etd,carrier.atd as atd,carrier.voyage as voyage,carrier.shippingLine as shippingLine FROM ConsolidationDetails consol "
        + " LEFT JOIN consol.carrierDetails carrier ")
    Page<IConsolidationDetailsResponse> findAllLiteConsol(Specification<ConsolidationDetails> spec, Pageable pageable);

    @Query(value = "SELECT consol.id as consolId, shipment.shipmentId as shipmentId, shipment.houseBill as houseBill  FROM ConsolidationDetails consol "
        + " LEFT JOIN consol.shipmentsList shipment")
    List<IShipmentResponse> findLiteShipmentFromConsol(Specification<ConsolidationDetails> spec, Pageable pageable);
}
