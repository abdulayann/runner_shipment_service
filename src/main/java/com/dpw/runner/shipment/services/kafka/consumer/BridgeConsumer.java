package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.DocumentDto;
import com.dpw.runner.shipment.services.kafka.dto.Event;
import com.dpw.runner.shipment.services.kafka.dto.TenantCallbackResponseDTO;
import com.dpw.runner.shipment.services.repository.interfaces.IGenericQueryRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;


@Service
@Slf4j
@Generated
public class BridgeConsumer {
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private ICustomerBookingDao customerBookingDao;
    @Autowired
    private IAuditLogService auditLogService;

    @KafkaListener(
            topics = "${bridge.event.kafka.queue}",
            autoStartup = "${bridge.event.kafka.consumer-auto-startup}",
            groupId = "${bridge.event.kafka.subs}"
    )
    public void consume(String message) {
        try {
            TenantCallbackResponseDTO responseDTO = objectMapper.readValue(
                    StringEscapeUtils.unescapeJson(message), TenantCallbackResponseDTO.class
            );

            Map<String, Object> extraParams = responseDTO.getExtraResponseParams();
            if (extraParams == null || !extraParams.containsKey("SERVICE_REQUEST_DATA")) {
                log.error("SERVICE_REQUEST_DATA is missing!");
                return;
            }

            JsonNode serviceRequestNode = objectMapper.readTree((String) extraParams.get("SERVICE_REQUEST_DATA"));

            if (responseDTO.getRequestCode().endsWith("HBL") || responseDTO.getRequestCode().endsWith("POD")) {
                processHblOrPodRequest(serviceRequestNode);
            } else if (responseDTO.getRequestCode().endsWith("EVENTS")) {
                processEvents(serviceRequestNode);
            }

        } catch (Exception e) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}",
                    LoggerEvent.KAFKA_BRIDGE_EVENT, message, e.getLocalizedMessage(), e);
        }
    }

    private void processHblOrPodRequest(JsonNode serviceRequestNode) {
        String bookingNumber = serviceRequestNode.path("BookingNumber").asText();
        String type = serviceRequestNode.path("Type").asText();

        Optional<CustomerBooking> customerBooking = customerBookingDao.findByBookingNumber(bookingNumber);
        if (customerBooking.isEmpty()) {
            log.warn("No customer booking found for BookingNumber: {}", bookingNumber);
            return;
        }

        Integer tenantId = customerBooking.get().getTenantId();
        setTenantContext(tenantId);

        if (StringUtility.isEmpty(bookingNumber)) {
            log.warn("Booking number is empty.");
            return;
        }

        try {
            Optional<CustomerBooking> booking = customerBookingDao.findByBookingNumberQuery(bookingNumber);
            if (booking.isPresent()) {
                auditLogService.addAuditLog(createAuditLog(tenantId, booking.get().getId(),
                        type.equalsIgnoreCase("HBL") ? "Doc: HBL" : "Doc: POD"));
            }
        } catch (Exception e) {
            log.error("Error processing HBL/POD request: {}", e.getMessage(), e);
        } finally {
            clearTenantContext();
        }
    }

    private void processEvents(JsonNode serviceRequestNode) throws JsonProcessingException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        JsonNode dataArray = serviceRequestNode.get("data");
        if (dataArray == null || !dataArray.isArray()) {
            log.error("No events found in 'data'.");
            return;
        }

        List<Event> events = objectMapper.readValue(dataArray.toString(), new TypeReference<List<Event>>() {});
        if (events.isEmpty()) {
            log.warn("Empty events list received.");
            return;
        }

        for (Event event : events) {
            Optional<CustomerBooking> booking = customerBookingDao.findByBookingNumberQuery(event.getBookingNumber());
            if (booking.isPresent()) {
                auditLogService.addAuditLog(createAuditLog(
                        booking.get().getTenantId(), booking.get().getId(), "Event: " + event.getEventCode()
                ));
            } else {
                log.warn("No booking found for event: {}", event);
            }
        }
    }

    private AuditLogMetaData createAuditLog(Integer tenantId, Long parentId, String dataType) {
        return AuditLogMetaData.builder()
                .tenantId(tenantId)
                .userName(UserContext.getUser().getUsername())
                .newData(new BaseEntity())
                .prevData(null)
                .parent(CustomerBooking.class.getSimpleName())
                .parentId(parentId)
                .isIntegrationLog(true)
                .flow(Constants.OUTBOUND)
                .dataType(dataType)
                .operation(DBOperationType.CREATE.name())
                .build();
    }

    private void setTenantContext(Integer tenantId) {
        RequestAuthContext.setAuthToken("Bearer " + StringUtils.defaultString(v1Service.generateToken()));
        TenantContext.setCurrentTenant(tenantId);
        UserContext.setUser(UsersDto.builder().TenantId(tenantId).Permissions(new HashMap<>()).build());
    }

    private void clearTenantContext() {
        TenantContext.removeTenant();
        UserContext.removeUser();
        RequestAuthContext.removeToken();
    }
}
