package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.Hbl;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IAwbDao {
    Awb save(Awb awbShipmentInfo);
    Page<Awb> findAll(Specification<Awb> spec, Pageable pageable);
    Optional<Awb> findById(Long id);
    List<Awb> findByShipmentId(Long shipmentId);
    List<Awb> findByConsolidationId(Long consolidationId);

    List<Awb> findByIssuingAgent(String issuingAgent);
    List<Awb> findByAwbNumber(List<String> awbNumber);
    List<Awb> findByAwbNumberAndIssuingAgent(List<String> awbNumber, String issuingAgent);
    List<Awb> saveAll(List<Awb> req);
}
