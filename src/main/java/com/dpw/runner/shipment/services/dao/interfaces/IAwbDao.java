package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.enums.PrintType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
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
    List<Awb> findByShipmentIdList(List<Long> shipmentIds);

    List<Awb> findByShipmentIdByQuery(Long shipmentId);
    List<Awb> findByConsolidationIdByQuery(Long consolidationId);

    List<Awb> findByIssuingAgent(String issuingAgent);
    List<Awb> findByAwbNumber(List<String> awbNumber);
    List<Awb> findByAwbNumberAndIssuingAgent(List<String> awbNumber, String issuingAgent);
    List<Awb> saveAll(List<Awb> req);
    void airMessagingIntegration(Long id, String reportType, Boolean fromShipment, boolean includeCSD);
    int updateAirMessageStatus(UUID guid, String airMessageStatus);
    int updateLinkedHawbAirMessageStatus(UUID guid, String airMessageStatus);

    int updateUserDetails(UUID guid, String userDisplayName, String userMailId);
    List<Awb> findAllLinkedAwbs(UUID guid);
    Awb findAwbByGuidByQuery(UUID guid);
    int updateAirMessageStatusFromShipmentId(Long id, String airMessageStatus);
    int updateAirMessageStatusFromConsolidationId(Long id, String airMessageStatus);
    int updatePrintTypeFromConsolidationId(Long id, String printType, Boolean isOriginal, LocalDateTime printedAt);
    int updatePrintTypeFromShipmentId(Long id, String printType, Boolean isOriginal, LocalDateTime printedAt);
    List<Awb> findByShipmentIdsByQuery(List<Long> shipmentIds);
    void updatedAwbInformationEvent(Object newEntity, Object oldEntity) throws RunnerException;
    void updateSciFieldFromHawb(Awb awb, Awb oldEntity, boolean isReset, Long awbId) throws RunnerException;
    List<Awb> getLinkedAwbFromMawb(Long mawbId);
    void updateAwbPrintInformation(Long shipmentId, Long consolidationId, PrintType printType, Boolean isOriginal, LocalDateTime printedAt);
    List<Awb> findByIds(List<Long> id);
    List<Awb> findAwbByAwbNumbers(List<String> awbNumbers);
    void validateAirMessaging(Long id) throws RunnerException;
}
