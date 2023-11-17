package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.Kafka.Dto.AwbShipConsoleDto;
import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Slf4j
public class AwbDao implements IAwbDao {
    @Autowired
    private IAwbRepository awbRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private KafkaProducer producer;
    @Value("${awbKafka.queue}")
    private String senderQueue;
    @Autowired
    IShipmentDao shipmentDao;
    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;
    @Override
    public Awb save(Awb awbShipmentInfo) {
        boolean isCreate = false;
        if (awbShipmentInfo.getId() == null) {
            isCreate = true;
        }
        Awb awb = awbRepository.save(awbShipmentInfo);
        pushToKafka(awb, isCreate);
        return awb;
    }

    @Async
    private void pushToKafka(Awb awb, boolean isCreate) {
        try {
            AwbResponse awbResponse = jsonHelper.convertValue(awb, AwbResponse.class);
            if(awb.getShipmentId() != null && awb.getAwbShipmentInfo().getEntityType().equals(Constants.DMAWB)) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
                if(shipmentDetails.isPresent())
                    awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(shipmentDetails.get(), AwbShipConsoleDto.class));
                else
                    return;
            } else if (awb.getConsolidationId() != null) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
                if(consolidationDetails.isPresent())
                    awbResponse.setAwbKafkaEntity(jsonHelper.convertValue(consolidationDetails.get(), AwbShipConsoleDto.class));
                else
                    return;
            }
            else {
                return;
            }

            KafkaResponse kafkaResponse = producer.getKafkaResponse(awbResponse, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e)
        {
            log.error("Error pushing awb to kafka");
        }
    }

    @Override
    public Page<Awb> findAll(Specification<Awb> spec, Pageable pageable) {
        return awbRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Awb> findById(Long id) {
        return awbRepository.findById(id);
    }

    @Override
    public List<Awb> findByShipmentId(Long shipmentId) {
        return awbRepository.findByShipmentId(shipmentId);
    }

    @Override
    public List<Awb> findByConsolidationId(Long consolidationId) {return awbRepository.findByConsolidationId(consolidationId);}

    @Override
    public List<Awb> findByIssuingAgent(String issuingAgent) { return awbRepository.findByIssuingAgent(issuingAgent);}

    @Override
    public List<Awb> findByAwbNumber(List<String> awbNumber) { return awbRepository.findByAwbNumber(awbNumber);}

    @Override
    public List<Awb> findByAwbNumberAndIssuingAgent(List<String> awbNumber, String issuingAgent) {
        return awbRepository.findByAwbNumberAndIssuingAgent(awbNumber, issuingAgent);
    }

    @Override
    public List<Awb> saveAll(List<Awb> req) {
        return awbRepository.saveAll(req);
    }

}
