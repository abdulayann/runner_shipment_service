package com.dpw.runner.shipment.services.service.TO.impl;


import com.dpw.runner.shipment.services.entity.enums.MessageType;
import com.dpw.runner.shipment.services.entity.enums.StatusType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.TO.response.DescartesResponse;
import com.dpw.runner.shipment.services.service.TO.response.ExternalResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Base64;
import java.util.List;
import java.util.Objects;

@Service
@Slf4j
public class ExternalIntegrationService {

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    BridgeApiService bridgeApiService;

    @Autowired
    DescartesAdapterImpl descartesAdapter;

    @Autowired
    XmlReader xmlReader;

    @Autowired
    EmailService emailService;

    public void file(String xmlbase64, ExternalResponse externalResponse) {
        try {
            DescartesResponse res = descartesAdapter.pushAirWayBillDetails(xmlbase64);
            externalResponse.setXmlPayload(xmlbase64);

            String errors = findErrors(res);
            if (Objects.nonNull(errors)) {
                externalResponse.setStatus(StatusType.EXTERNAL_VALIDATION_ERROR);
                externalResponse.setMessage(errors);
            }
            else {
                externalResponse.setTid(res.getTid());
                externalResponse.setStatus(StatusType.SUBMITTED);
            }

        } catch (Exception ex) {
            emailService.sendEmail(ex.getMessage(), "Exception CONSUMER FILE EAWB DESCARTES  " , List.of("sumit.bansal@dpworld.com", "kushal.sharma1@dpworld.com"), null, null );
            log.error("Exception in ExternalIntegrationService file method: {}", ex.getMessage());
            externalResponse.setStatus(StatusType.INTERNAL_ERROR);
            externalResponse.setMessage(ex.getMessage());
        }
    }

    private String findErrors(DescartesResponse res) {
        if (Objects.nonNull(res.getError()) || Objects.nonNull(res.getErrorDetail()) || Objects.nonNull(res.getErrorShort()) || Objects.nonNull(res.getUnhandledException())
        || Objects.nonNull(res.getProcessingLog())) {
            String errors = res.getError() + "  " + res.getErrorDetail() + "  " + res.getErrorShort() + "  " + res.getUnhandledException();
            if (Objects.nonNull(res.getProcessingLog()))
                errors += " " + res.getProcessingLog().toString();

            return errors;
        }
        return null;
    }

    public void convertJsontoXML(MessageType type, Object payload, ExternalResponse externalResponse) {
        try {
            String xml = bridgeApiService.validateTemplate(xmlReader.readXmlTemplate(type.name()), jsonHelper.convertToJson(payload));
            externalResponse.setXmlPayload(new String(Base64.getEncoder().encode(xml.getBytes())));
        } catch (Exception ex) {
            externalResponse.setMessage(ex.getMessage());
            externalResponse.setStatus(StatusType.INTERNAL_ERROR);
        }
    }

}
