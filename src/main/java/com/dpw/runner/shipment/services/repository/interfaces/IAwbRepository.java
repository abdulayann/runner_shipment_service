package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.Hbl;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface IAwbRepository extends MultiTenancyRepository<Awb> {
    Page<Awb> findAll(Specification<Awb> spec, Pageable pageable);
    List<Awb> findByShipmentId(Long shipmentId);
    List<Awb> findByConsolidationId(Long shipmentId);

    @Query(value = "SELECT e FROM Awb e WHERE FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'issuingAgentName') = :issuingAgent")
    List<Awb> findByIssuingAgent(@Param("issuingAgent") String issuingAgent);

    @Query(value = "SELECT e FROM Awb e WHERE FUNCTION('jsonb_extract_path_text', e.awbShipmentInfo, 'awbNumber') = :awbNumber")
    List<Awb> findByAwbNumber(@Param("awbNumber") String awbNumber);
}
