package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IAwbDao {
    Awb save(Awb awbShipmentInfo) throws RunnerException;
    Page<Awb> findAll(Specification<Awb> spec, Pageable pageable);
    Optional<Awb> findById(Long id);

    Optional<Awb> findByGuid(UUID guid);
    List<Awb> findByShipmentId(Long shipmentId);
    List<Awb> findByConsolidationId(Long consolidationId);

    List<Awb> findByShipmentIdByQuery(Long shipmentId);
    List<Awb> findByConsolidationIdByQuery(Long consolidationId);

    List<Awb> findByIssuingAgent(String issuingAgent);
    List<Awb> findByAwbNumber(List<String> awbNumber);
    List<Awb> findByAwbNumberAndIssuingAgent(List<String> awbNumber, String issuingAgent);
    List<Awb> saveAll(List<Awb> req);
    void airMessagingIntegration(Long id, String reportType, Boolean fromShipment);
    int updateAirMessageStatus(UUID guid, String airMessageStatus);
    int updateLinkedHawbAirMessageStatus(UUID guid, String airMessageStatus);

    int updateUserDetails(UUID guid, String userDisplayName, String userMailId);
    List<Awb> findAllLinkedAwbs(UUID guid);
    Awb findAwbByGuidByQuery(UUID guid);
    int updateAirMessageStatusFromShipmentId(Long id, String airMessageStatus);
    int updateAirMessageStatusFromConsolidationId(Long id, String airMessageStatus);

}
