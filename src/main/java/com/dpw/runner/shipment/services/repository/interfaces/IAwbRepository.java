package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Repository
public interface IAwbRepository extends MultiTenancyRepository<Awb> {
    Page<Awb> findAll(Specification<Awb> spec, Pageable pageable);
    List<Awb> findByShipmentId(Long shipmentId);
    List<Awb> findByConsolidationId(Long shipmentId);
    Optional<Awb> findByGuid(UUID guid);

    @Query(value = "SELECT * FROM awb WHERE shipment_id = ?1", nativeQuery = true)
    List<Awb> findByShipmentIdByQuery(Long shipmentId);
    @Query(value = "SELECT * FROM awb WHERE consolidation_id = ?1", nativeQuery = true)
    List<Awb> findByConsolidationIdByQuery(Long consolidationId);
    Optional<Awb> findByGuid(UUID guid);

    @Query(value = "SELECT e FROM Awb e WHERE FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'issuingAgentName') = :issuingAgent")
    List<Awb> findByIssuingAgent(@Param("issuingAgent") String issuingAgent);

    @Query(value = "SELECT e FROM Awb e WHERE FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'awbNumber') in :awbNumber")
    List<Awb> findByAwbNumber(@Param("awbNumber") List<String> awbNumber);

    @Query(value = "SELECT e FROM Awb e WHERE FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'awbNumber') in :awbNumber" +
            " AND FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'issuingAgentName') = :issuingAgent")
    List<Awb> findByAwbNumberAndIssuingAgent(@Param("awbNumber") List<String> awbNumber, @Param("issuingAgent") String issuingAgent);

    @Transactional
    @Modifying
    @Query(value = "Update Awb set air_message_status = ?2 Where guid = ?1", nativeQuery = true)
    int updateAirMessageStatus(UUID guid, String airMessageStatus);

    @Transactional
    @Modifying
    @Query(value = "Update Awb set linked_hawb_air_message_status = ?2 Where guid = ?1", nativeQuery = true)
    int updateLinkedHawbAirMessageStatus(UUID guid, String airMessageStatus);

    @Transactional
    @Modifying
    @Query(value = "Update Awb set air_message_status = ?2 Where shipment_id = ?1", nativeQuery = true)
    int updateAirMessageStatusFromShipmentId(Long id, String airMessageStatus);

    @Transactional
    @Modifying
    @Query(value = "Update Awb set air_message_status = ?2 Where consolidation_id = ?1", nativeQuery = true)
    int updateAirMessageStatusFromConsolidationId(Long id, String airMessageStatus);

    @Transactional
    @Modifying
    @Query(value = "Update Awb set user_mail_id = ?3, user_display_name = ?2 Where guid = ?1", nativeQuery = true)
    int updateUserDetails(UUID guid, String userDisplayName, String userMailId);

    @Query(value = "SELECT * FROM awb WHERE guid = ?1 limit 1", nativeQuery = true)
    Awb findAwbByGuidByQuery(UUID guid);

}
