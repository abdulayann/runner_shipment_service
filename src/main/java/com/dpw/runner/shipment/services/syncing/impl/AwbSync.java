package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbHawbLinkDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IAwbSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Component
@Slf4j
@SuppressWarnings("ALL")
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

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private EmailServiceUtility emailServiceUtility;

    @Autowired
    private IAwbDao awbDao;

    @Autowired
    private IMawbHawbLinkDao mawbHawbLinkDao;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.awbSync}")
    private String AWB_V1_SYNC_URL;
    @Autowired
    private ISyncService syncService;

    @Async
    @Override
    public void sync(Awb awb, SaveStatus saveStatus) {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return;

        boolean isMawb = awb.getAwbShipmentInfo().getEntityType().equalsIgnoreCase("mawb");
        AwbRequestV2 awbRequest = generateAwbSyncRequest(awb);
        awbRequest.setSaveStatus(saveStatus);
        List<Awb> linkedHawb;
        if(isMawb) {
            linkedHawb = getLinkedAwbFromMawb(awb.getId());
        } else {
            linkedHawb = new ArrayList<>();
        }
        String transactionId = String.valueOf(awb.getGuid());
        String finalAwb = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(awbRequest).module(SyncingConstants.AWB).build());
        syncService.pushToKafka(finalAwb, String.valueOf(awb.getId()), String.valueOf(awb.getGuid()), SyncingConstants.AWB, transactionId);
        linkedHawb.forEach(i -> {
            AwbRequestV2 hawbSyncRequest = generateAwbSyncRequest(i);
            String finalHawb = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(hawbSyncRequest).module(SyncingConstants.AWB).build());
            syncService.pushToKafka(finalHawb, String.valueOf(i.getId()), String.valueOf(i.getGuid()), SyncingConstants.AWB, transactionId);
        });
        log.info("Completed AWB Sync with data: {}", modelMapper.map(finalAwb, HblDataRequestV2.class));
    }

    private AwbRequestV2 convertEntityToDto(Awb awb) {
        AwbRequestV2 res = jsonHelper.convertValue(awb, AwbRequestV2.class);
        res.setAwbNotifyPartyInfo(convertToList(awb.getAwbNotifyPartyInfo(), AwbNotifyPartyInfoV2.class));
        res.setAwbRoutingInfo(convertToList(awb.getAwbRoutingInfo(), AwbRoutingInfoV2.class));
        res.setAwbOtherChargesInfo(convertToList(awb.getAwbOtherChargesInfo(), AwbOtherChargesInfoV2.class));
       // res.setAwbOciInfo(convertToList(awb.getAwbOciInfo(), AwbOCIInfoV2.class));
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
                .toList();
    }

    private AwbRequestV2 generateAwbSyncRequest(Awb awb) {
        AwbRequestV2 awbRequest = convertEntityToDto(awb);
        if(awb.getShipmentId() != null){
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
            awbRequest.setShipmentGuid(shipmentDetails.get().getGuid());
        }
        if(awb.getConsolidationId() != null){
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
            awbRequest.setConsolidationGuid(consolidationDetails.get().getGuid());
        }
        return  awbRequest;
    }

    List<Awb> getLinkedAwbFromMawb(Long mawbId) {
        List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(mawbId);

        // Fetch all the awb records with the mapped hawbId
        ListCommonRequest listCommonRequest = CommonUtils.constructListCommonRequest("id", mawbHawbLinks.stream().map(i -> i.getHawbId()).toList(), "IN");
        Pair<Specification<Awb>, Pageable> pair = fetchData(listCommonRequest, Awb.class);
        Page<Awb> page = awbDao.findAll(pair.getLeft(), pair.getRight());

        List<Awb> linkedHawb = new ArrayList<>();
        if(!page.isEmpty())
            linkedHawb = page.getContent();

        return linkedHawb;
    }

}
