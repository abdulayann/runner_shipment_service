package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.DpsEvent.DpsFieldData;
import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Strings;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class DpsEventService implements IDpsEventService {

    @Autowired
    private IDpsEventRepository dpsEventRepository;

    @Override
    public DpsEvent saveDpsEvent(DpsDto dpsDto){
        DpsEvent dpsEvent = constructDpsEvent(dpsDto);
        return dpsEventRepository.save(dpsEvent);
    }

    @Override
    public List<DpsEvent> getMatchingRulesByGuid(CommonRequestModel commonRequestModel) {

        try {
            CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
            String guid = commonGetRequest.getGuid();
            if (Strings.isNullOrEmpty(guid)) {
                log.error("GUID is null for DpsEvent retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DpsException("GUID can't be null. Please provide guid!");
            }
            return dpsEventRepository.findDpsEventByGuidAndExecutionState(guid, DpsExecutionStatus.ACTIVE);
        } catch (Exception e) {
            throw new DpsException("Error in fetching object of DpsEvent: " + e.getMessage(), e);
        }
    }

    private DpsEvent constructDpsEvent(DpsDto dpsDto) {
        try {

            return new DpsEvent()
                    .setExecutionId(dpsDto.getExecutionId())
                    .setEntityId(dpsDto.getEntityId())
                    .setWorkflowType(DpsWorkflowType.valueOf(dpsDto.getWorkflowType()))
                    .setState(DpsWorkflowState.valueOf(dpsDto.getState()))
                    .setStatus(DpsExecutionStatus.valueOf(dpsDto.getStatus()))
                    .setEntityType(DpsEntityType.valueOf(dpsDto.getEntityType()))
                    .setText(dpsDto.getText())
                    .setImplicationList(dpsDto.getImplications())
                    .setConditionMessageList(dpsDto.getConditionMessage())
                    .setDpsFieldData(createDpsFieldDataList(
                            dpsDto.getFieldsDetected(), dpsDto.getFieldsDetectedValues()));

        } catch (Exception e) {
            throw new DpsException("Error in creating object of DpsEvent: " + e.getMessage(), e);
        }
    }

    private List<DpsFieldData> createDpsFieldDataList(List<String> keys, List<String> values) {
        List<DpsFieldData> dpsFieldDataList = new ArrayList<>();

        for (int i = 0; i < keys.size(); i++) {
            dpsFieldDataList.add(new DpsFieldData(keys.get(i), values.get(i)));
        }

        return dpsFieldDataList;
    }


}
