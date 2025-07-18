package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.projection.CustomerBookingProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
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

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;


@Repository
@Generated
@InterBranchEntity
public interface IShipmentRepository extends MultiTenancyRepository<ShipmentDetails> {
    List<ShipmentDetails> findAll();

    Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable);

    default Optional<ShipmentDetails> findById(Long id) {
        Specification<ShipmentDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    default Optional<ShipmentDetails> findByGuid(UUID id) {
        Specification<ShipmentDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("guid"), id);
        return findOne(spec);
    }

    @ExcludeTenantFilter
    @Query(value = "SELECT * FROM shipment_details WHERE guid IN ?1", nativeQuery = true)
    List<ShipmentDetails> findAllByGuids(List<UUID> guids);

    @Query(value = "SELECT * FROM shipment_details where house_bill = ?1 and tenant_id = ?2", nativeQuery = true)
    List<ShipmentDetails> findByHouseBill(String hbl, Integer tenantId);

    List<ShipmentDetails> findAllByHouseBill(String hbl);

    @Query(value = "SELECT * FROM shipment_details where booking_reference = ?1 and tenant_id = ?2", nativeQuery = true)
    List<ShipmentDetails> findByBookingReference(String ref, Integer tenantId);

    @Query(value = "SELECT MAX(c.id) FROM consolidation_details c", nativeQuery = true)
    Long findMaxId();

    @Modifying
    @Transactional
    @Query(value = "Update shipment_details set job_status = ?2 Where id = ?1", nativeQuery = true)
    void saveJobStatus(Long id, String jobStatus);

    @Modifying
    @Transactional
    @ExcludeTenantFilter
    @Query(value = "Update shipment_details set status = ?2 Where id = ?1", nativeQuery = true)
    void saveStatus(Long id, Integer status);

    @Modifying
    @Transactional
    @Query(value = "Update shipment_details set created_by = ?2, created_at = ?3 Where id = ?1", nativeQuery = true)
    void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate);

    @Query(value = "SELECT * FROM shipment_details WHERE id IN ?1", nativeQuery = true)
    List<ShipmentDetails> getShipmentNumberFromId(List<Long> shipmentIds);

    @Modifying
    @Transactional
    @Query(value = "Update shipment_details set entity_transfer = ?2 Where id = ?1", nativeQuery = true)
    void saveEntityTransfer(Long id, Boolean entityTransfer);

    @Query(value = "SELECT * FROM shipment_details WHERE guid IN ?1", nativeQuery = true)
    List<ShipmentDetails> findShipmentsByGuids(Set<UUID> guids);

    @Query(value = "SELECT * FROM shipment_details WHERE source_guid IN ?1", nativeQuery = true)
    List<ShipmentDetails> findShipmentsBySourceGuids(Set<UUID> sourceGuid);

    @Query(value = "SELECT * FROM shipment_details WHERE source_guid = ?1 and tenant_id = ?2 and is_deleted = false", nativeQuery = true)
    List<ShipmentDetails> findShipmentBySourceGuidAndTenantId(UUID sourceGuid, Integer tenantId);

    @Query(value = "SELECT * FROM shipment_details WHERE id IN ?1", nativeQuery = true)
    List<ShipmentDetails> findShipmentsByIds(Set<Long> id);

    @Query(value = "SELECT * FROM shipment_details WHERE id = ?1", nativeQuery = true)
    Optional<ShipmentDetails> findShipmentByIdWithQuery(Long id);

    List<ShipmentDetails> findBySourceGuid(UUID guid);

    @Query(value = "SELECT distinct s.id from ShipmentDetails s inner join ConsoleShipmentMapping csm " +
            "on s.id = csm.shipmentId " +
            "where csm.isAttachmentDone = false and csm.requestedType = ?1")
    Page<Long> getIdWithPendingActions(ShipmentRequestedType shipmentRequestedType, Pageable pageable);

    @Query(value = "SELECT "
            + " tenant_id as tenantId, "
            + " house_bill as hblNumber, "
            + " shipment_id as shipmentId "
            + " FROM shipment_details "
            + " WHERE house_bill = ?1 "
            + " AND (?2 IS NULL OR shipment_id != CAST(?2 AS VARCHAR))", nativeQuery = true)
    List<ShipmentDetailsProjection> findByHblNumberAndExcludeShipmentId(String hblNumber, String shipmentId);

    @Query(value = "SELECT sd.id as id, "
            + " sd.shipment_id as shipmentNumber, "
            + " scm.container_id as containerId, "
            + " sd.shipment_type as shipmentType"
            + " FROM shipment_details sd"
            + " JOIN shipments_containers_mapping scm ON sd.id = scm.shipment_id"
            + " WHERE scm.container_id in (?1)", nativeQuery = true)
    List<ShipmentDetailsProjection> findShipmentDetailsByAttachedContainerIds(List<Long> containerIds);

    @Modifying
    @Transactional
    @Query(value = "Update shipment_details set is_transferred_to_receiving_branch = ?2 Where id = ?1", nativeQuery = true)
    void saveIsTransferredToReceivingBranch(Long id, Boolean entityTransferred);

    @Modifying
    @Transactional
    @Query(value = "Update triangulation_partner_shipment set is_accepted = ?3 where shipment_id = ?1 AND partner_id = ?2", nativeQuery = true)
    void updateIsAcceptedTriangulationPartner(Long shipmentId, Long triangulationPartner, Boolean isAccepted);

    @ExcludeTenantFilter
    default Page<ShipmentDetails> findAllWithoutTenantFilter(Specification<ShipmentDetails> spec, Pageable pageable) {
        return findAll(spec, pageable);
    }

    @ExcludeTenantFilter
    List<ShipmentDetails> findByShipmentId(String shipmentNumber);

    @Modifying
    @Transactional
    @ExcludeTenantFilter
    @Query(value = "UPDATE shipment_additional_details a " +
            "SET empty_container_returned = ?2 " +
            "FROM shipment_details s " +
            "WHERE s.additional_details_id = a.id AND s.id = ?1", nativeQuery = true)
    void updateAdditionalDetailsByShipmentId(Long id, Boolean emptyContainerReturned);

    @Query(value = "SELECT * FROM shipment_details WHERE id IN ?1 AND contains_hazardous = ?2", nativeQuery = true)
    List<ShipmentDetails> findByShipmentIdInAndContainsHazardous(List<Long> shipmentIdList, boolean containsHazardous);

    List<ShipmentDetails> findByShipmentIdIn(List<String> shipmentIds);

    @Modifying
    @Query(value = "update shipment_additional_details set fcr_number = fcr_number + 1 where id in (select additional_details_id from shipment_details where id = ?1)", nativeQuery = true)
    void updateFCRNo(Long id);

    @Query(value = "SELECT * FROM shipment_details WHERE guid = ?1", nativeQuery = true)
    Optional<ShipmentDetails> findShipmentByGuidWithQuery(UUID guid);

    @Query(value = "SELECT sd.* FROM shipment_details sd "+
            "LEFT JOIN console_shipment_mapping csm " +
            "ON sd.id = csm.shipment_id " +
            "WHERE csm.shipment_id IS NULL and sd.migration_status IN (:statuses) and sd.tenant_id = :tenantId and sd.is_deleted = false", nativeQuery = true)
    List<ShipmentDetails> findAllByMigratedStatuses(@Param("statuses") List<String> migrationStatuses, @Param("tenantId") Integer tenantId);

    @Modifying
    @Query(value = "Update shipment_details set booking_number = ?2 Where guid IN ?1", nativeQuery = true)
    int updateShipmentsBookingNumber(List<UUID> guids, String bookingNumber);

    @ExcludeTenantFilter
    @Query(value = "SELECT receiving_branch FROM shipment_details WHERE guid = ?1", nativeQuery = true)
    Integer findReceivingByGuid(UUID guid);

    @Modifying
    @Transactional
    @Query("UPDATE ShipmentDetails s SET " +
            "s.noOfPacks = :noOfPacks, " +
            "s.packsUnit = :packsUnit, " +
            "s.volume = :volume, " +
            "s.volumeUnit = :volumeUnit, " +
            "s.weight = :weight, " +
            "s.weightUnit = :weightUnit, " +
            "s.volumetricWeight = :volumetricWeight, " +
            "s.volumetricWeightUnit = :volumetricWeightUnit, " +
            "s.chargable = :chargable, " +
            "s.chargeableUnit = :chargeableUnit " +
            "WHERE s.id = :shipmentId")
    int updateCargoDetailsInShipment(
            @Param("shipmentId") Long shipmentId,
            @Param("noOfPacks") Integer noOfPacks,
            @Param("packsUnit") String packsUnit,
            @Param("volume") BigDecimal volume,
            @Param("volumeUnit") String volumeUnit,
            @Param("weight") BigDecimal weight,
            @Param("weightUnit") String weightUnit,
            @Param("volumetricWeight") BigDecimal volumetricWeight,
            @Param("volumetricWeightUnit") String volumetricWeightUnit,
            @Param("chargable") BigDecimal chargable,
            @Param("chargeableUnit") String chargeableUnit
    );

    @Modifying
    @Query("UPDATE ShipmentDetails s SET s.containerAssignedToShipmentCargo = :containerId WHERE s.id IN :shipmentIds")
    void setShipmentIdsToContainer(@Param("shipmentIds") List<Long> shipmentIds, @Param("containerId") Long containerId);

    @Modifying
    @Transactional
    @Query("""
            UPDATE ShipmentDetails s
            SET s.terminalCutoff = :terminalCutoff,
                s.verifiedGrossMassCutoff = :verifiedGrossMassCutoff,
                s.shippingInstructionCutoff = :shippingInstructionCutoff,
                s.dgCutoff = :dgCutoff,
                s.reeferCutoff = :reeferCutoff,
                s.earliestEmptyEquipmentPickUp = :earliestEmptyEquipmentPickUp,
                s.latestFullEquipmentDeliveredToCarrier = :latestFullEquipmentDeliveredToCarrier,
                s.earliestDropOffFullEquipmentToCarrier = :earliestDropOffFullEquipmentToCarrier
            WHERE s.id = :shipmentId
            """)
    void updateSailingScheduleRelatedInfo(
            @Param("shipmentId") Long shipmentId,
            @Param("terminalCutoff") LocalDateTime terminalCutoff,
            @Param("verifiedGrossMassCutoff") LocalDateTime verifiedGrossMassCutoff,
            @Param("shippingInstructionCutoff") LocalDateTime shippingInstructionCutoff,
            @Param("dgCutoff") LocalDateTime dgCutoff,
            @Param("reeferCutoff") LocalDateTime reeferCutoff,
            @Param("earliestEmptyEquipmentPickUp") LocalDateTime earliestEmptyEquipmentPickUp,
            @Param("latestFullEquipmentDeliveredToCarrier") LocalDateTime latestFullEquipmentDeliveredToCarrier,
            @Param("earliestDropOffFullEquipmentToCarrier") LocalDateTime earliestDropOffFullEquipmentToCarrier
    );

    @Modifying
    @Transactional
    @Query("""
            UPDATE ShipmentDetails s
            SET s.latestArrivalTime = :latestArrivalTime
            WHERE s.id = :shipmentId
            """)
    void updateSailingScheduleRelatedInfoForAir(@Param("shipmentId") Long shipmentId, @Param("latestArrivalTime") LocalDateTime latestArrivalTime);

    @Modifying
    @Transactional
    @Query("UPDATE ShipmentDetails s SET " +
            "s.dateType = :dateType, " +
            "s.shipmentGateInDate = :shipmentGateInDate, " +
            "s.shipmentPackStatus = :shipmentPackStatus " +
            "WHERE s.id = :shipmentId")
    void updateShipmentDetailsFromPacks(
            @Param("shipmentId") Long shipmentId,
            @Param("dateType") DateBehaviorType dateType,
            @Param("shipmentGateInDate") LocalDateTime shipmentGateInDate,
            @Param("shipmentPackStatus") ShipmentPackStatus shipmentPackStatus
    );


    @Query(value = """
            select cb.id as id,
            sd.id as shipmentId
             from ShipmentDetails sd inner join CustomerBooking cb on cb.bookingNumber = sd.bookingReference where sd.id in (:shipmentIdList)
            """)
    List<CustomerBookingProjection> findCustomerBookingProByShipmentIdIn(@Param("shipmentIdList") List<Long> shipmentIdList);

    @Modifying
    @Transactional
    @Query("UPDATE ShipmentDetails s SET " +
            "s.dgPacksCount = :dgPacks, " +
            "s.dgPacksUnit = :dgPacksUnit " +
            "WHERE s.id = :shipmentId")
    void updateDgPacksDetailsInShipment(@Param("dgPacks") Integer dgPacks, @Param("dgPacksUnit") String dgPacksUnit, @Param("shipmentId") Long shipmentId);

    @Modifying
    @Transactional
    @Query(value = "UPDATE shipment_details s " +
            "SET contains_hazardous = :isHazardous, " +
            "ocean_dg_status = :oceanDGStatus " +
            "WHERE id = :shipmentId", nativeQuery = true)
    void updateDgStatusInShipment(@Param("isHazardous") Boolean isHazardous,
                                  @Param("oceanDGStatus") String oceanDGStatus,
                                  @Param("shipmentId") Long shipmentId);
}
