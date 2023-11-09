package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.interfaces.IAwbSync;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@Slf4j
public class AwbSync implements IAwbSync {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    RestTemplate restTemplate;

    @Autowired
    ModelMapper modelMapper;

    @Autowired
    IShipmentDao shipmentDao;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.awbSync}")
    private String AWB_V1_SYNC_URL;

    @Override
    @Async
    public ResponseEntity<?> sync(Awb awb) {
        AwbRequestV2 awbRequest = convertEntityToDto(awb);
        if(awb.getShipmentId() != null){
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
            awbRequest.setShipmentGuid(shipmentDetails.get().getGuid());
        }
        if(awb.getConsolidationId() != null){
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
            awbRequest.setConsolidationGuid(consolidationDetails.get().getGuid());
        }

        String finalHbl = jsonHelper.convertToJson(awbRequest);
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if(ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}",ctx.getLastThrowable().getMessage());
            }
            HttpEntity<V1DataResponse> entity = new HttpEntity(finalHbl, V1AuthHelper.getHeaders());
            var response = this.restTemplate.postForEntity(this.AWB_V1_SYNC_URL, entity, V1DataResponse.class, new Object[0]);
            return response;
        });
        return ResponseHelper.buildSuccessResponse(modelMapper.map(finalHbl, HblDataRequestV2.class));
    }

    private AwbRequestV2 convertEntityToDto(Awb awb) {
        AwbRequestV2 res = jsonHelper.convertValue(awb, AwbRequestV2.class);
        res.setAwbNotifyPartyInfo(convertToList(awb.getAwbNotifyPartyInfo(), AwbNotifyPartyInfoV2.class));
        res.setAwbRoutingInfo(convertToList(awb.getAwbRoutingInfo(), AwbRoutingInfoV2.class));
        res.setAwbOtherChargesInfo(convertToList(awb.getAwbOtherChargesInfo(), AwbOtherChargesInfoV2.class));
        res.setAwbOciInfo(convertToList(awb.getAwbOciInfo(), AwbOCIInfoV2.class));
        res.setAwbGoodsDescriptionInfo(convertToList(awb.getAwbGoodsDescriptionInfo(), AwbGoodsDescriptionInfoV2.class));
        res.setAwbPackingInfo(convertToList(awb.getAwbPackingInfo(), AwbPackingInfoV2.class));
        res.setAwbSpecialHandlingCodesMappings(convertToList(awb.getAwbSpecialHandlingCodesMappings(), AwbSpecialHandlingCodesMappingInfoV2.class));
        return res;
    }
    private <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> jsonHelper.convertValue(item, clazz))
                .collect(Collectors.toList());
    }
}
