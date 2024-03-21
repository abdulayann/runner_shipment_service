package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Awb;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

public interface IAwbRepository extends MultiTenancyRepository<Awb> {
    Page<Awb> findAll(Specification<Awb> spec, Pageable pageable);
    List<Awb> findByShipmentId(Long shipmentId);
    List<Awb> findByConsolidationId(Long shipmentId);

    @Query(value = "SELECT e FROM Awb e WHERE FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'issuingAgentName') = :issuingAgent")
    List<Awb> findByIssuingAgent(@Param("issuingAgent") String issuingAgent);

    @Query(value = "SELECT e FROM Awb e WHERE FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'awbNumber') in :awbNumber")
    List<Awb> findByAwbNumber(@Param("awbNumber") List<String> awbNumber);

    @Query(value = "SELECT e FROM Awb e WHERE FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'awbNumber') in :awbNumber" +
            " AND FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'issuingAgentName') = :issuingAgent")
    List<Awb> findByAwbNumberAndIssuingAgent(@Param("awbNumber") List<String> awbNumber, @Param("issuingAgent") String issuingAgent);

    @Transactional
    @Modifying
    @Query(value = "Update Awb set is_air_messaging_sent = ?2 Where guid = ?1", nativeQuery = true)
    int updateIsAirMessagingSent(UUID guid, Boolean isAirMessagingSent);
}
