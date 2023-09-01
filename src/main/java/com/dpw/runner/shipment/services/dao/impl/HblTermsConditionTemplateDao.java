package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IHblTermsConditionTemplateDao;
import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;
import com.dpw.runner.shipment.services.repository.interfaces.IHblTermsConditionTemplateRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class HblTermsConditionTemplateDao implements IHblTermsConditionTemplateDao {

    @Autowired
    private IHblTermsConditionTemplateRepository hblTermsConditionTemplateRepository;

    @Override
    public HblTermsConditionTemplate save(HblTermsConditionTemplate hblTermsConditionTemplate) {
        return hblTermsConditionTemplateRepository.save(hblTermsConditionTemplate);
    }

    private Optional<HblTermsConditionTemplate> findById(Long id) {
        return hblTermsConditionTemplateRepository.findById(id);
    }

    private void delete(HblTermsConditionTemplate hblTermsConditionTemplate) {
        hblTermsConditionTemplateRepository.delete(hblTermsConditionTemplate);
    }

    @Override
    public List<HblTermsConditionTemplate> saveAll(List<HblTermsConditionTemplate> hblTermsConditionTemplateList) {
        return hblTermsConditionTemplateRepository.saveAll(hblTermsConditionTemplateList);
    }

    private Page<HblTermsConditionTemplate> findAll(Specification<HblTermsConditionTemplate> spec, Pageable pageable) {
        return hblTermsConditionTemplateRepository.findAll(spec, pageable);
    }

    @Override
    public List<HblTermsConditionTemplate> saveEntityFromSettings(List<HblTermsConditionTemplate> hblTermsConditionTemplateList, Long shipmentSettingsId, Boolean isFrontPrint) {
        List<HblTermsConditionTemplate> res = new ArrayList<>();
        for (HblTermsConditionTemplate req : hblTermsConditionTemplateList) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<HblTermsConditionTemplate> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Hbl Terms Condition Template is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentSettingsId(shipmentSettingsId);
            req.setIsFrontPrint(isFrontPrint);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    @Override
    public List<HblTermsConditionTemplate> updateEntityFromSettings(List<HblTermsConditionTemplate> hblTermsConditionTemplateList, Long shipmentSettingsId, Boolean isFrontPrint) throws Exception {
        String responseMsg;
        List<HblTermsConditionTemplate> responseHblTermsConditionTemplate = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentSettingsId", shipmentSettingsId, "=");
            Pair<Specification<HblTermsConditionTemplate>, Pageable> pair = fetchData(listCommonRequest, HblTermsConditionTemplate.class);
            Page<HblTermsConditionTemplate> hblTermsConditionTemplates = findAll(pair.getLeft(), pair.getRight());
            Map<Long, HblTermsConditionTemplate> hashMap = hblTermsConditionTemplates.stream()
                    .filter(e -> e.getIsFrontPrint() == isFrontPrint)
                    .collect(Collectors.toMap(HblTermsConditionTemplate::getId, Function.identity()));
            List<HblTermsConditionTemplate> hblTermsConditionTemplatesRequestList = new ArrayList<>();
            if (hblTermsConditionTemplateList != null && hblTermsConditionTemplateList.size() != 0) {
                for (HblTermsConditionTemplate request : hblTermsConditionTemplateList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    hblTermsConditionTemplatesRequestList.add(request);
                }
                responseHblTermsConditionTemplate = saveEntityFromSettings(hblTermsConditionTemplatesRequestList, shipmentSettingsId, isFrontPrint);
            }
            deleteHblTermsConditionTemplate(hashMap);
            return responseHblTermsConditionTemplate;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    private void deleteHblTermsConditionTemplate(Map<Long, HblTermsConditionTemplate> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }
}
