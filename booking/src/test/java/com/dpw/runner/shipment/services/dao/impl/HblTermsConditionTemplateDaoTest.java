package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IHblTermsConditionTemplateDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.HblTermsConditionTemplate;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.repository.interfaces.IHblTermsConditionTemplateRepository;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import javax.swing.text.html.Option;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class HblTermsConditionTemplateDaoTest {

    @InjectMocks
    private HblTermsConditionTemplateDao hblTermsConditionTemplateDao;
    @Mock
    private IHblTermsConditionTemplateRepository hblTermsConditionTemplateRepository;

    @Test
    void save() {
        HblTermsConditionTemplate hblTermsConditionTemplate = new HblTermsConditionTemplate();
        when(hblTermsConditionTemplateRepository.save(any())).thenReturn(hblTermsConditionTemplate);
        assertEquals(hblTermsConditionTemplate, hblTermsConditionTemplateDao.save(hblTermsConditionTemplate));
    }

    @Test
    void saveAll() {
        HblTermsConditionTemplate hblTermsConditionTemplate = new HblTermsConditionTemplate();
        List<HblTermsConditionTemplate> hblTermsConditionTemplateList = Arrays.asList(hblTermsConditionTemplate);
        when(hblTermsConditionTemplateRepository.saveAll(any())).thenReturn(hblTermsConditionTemplateList);
        assertEquals(hblTermsConditionTemplateList, hblTermsConditionTemplateDao.saveAll(hblTermsConditionTemplateList));
    }

    @Test
    void saveEntityFromSettings() {
        HblTermsConditionTemplate hblTermsConditionTemplate = new HblTermsConditionTemplate();
        hblTermsConditionTemplate.setId(1L);
        List<HblTermsConditionTemplate> hblTermsConditionTemplateList = Arrays.asList(hblTermsConditionTemplate);
        when(hblTermsConditionTemplateRepository.findById(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> {
                hblTermsConditionTemplateDao.saveEntityFromSettings(hblTermsConditionTemplateList, 1L, true);
        });
    }

    @Test
    void saveEntityFromSettingsEntityPresent() {
        HblTermsConditionTemplate hblTermsConditionTemplate = new HblTermsConditionTemplate();
        hblTermsConditionTemplate.setId(1L);
        List<HblTermsConditionTemplate> hblTermsConditionTemplateList = Arrays.asList(hblTermsConditionTemplate);
        when(hblTermsConditionTemplateRepository.findById(any())).thenReturn(Optional.of(hblTermsConditionTemplate));
        when(hblTermsConditionTemplateRepository.save(any())).thenReturn(hblTermsConditionTemplate);
        assertEquals(hblTermsConditionTemplateList, hblTermsConditionTemplateDao.saveEntityFromSettings(hblTermsConditionTemplateList, 1L, true));
    }

    @Test
    void updateEntityFromSettingsCatch() throws RunnerException {
        HblTermsConditionTemplate hblTermsConditionTemplate = new HblTermsConditionTemplate();
        hblTermsConditionTemplate.setIsFrontPrint(true);
        hblTermsConditionTemplate.setId(1L);
        List<HblTermsConditionTemplate> hblTermsConditionTemplateList = new ArrayList<>();
        hblTermsConditionTemplateList.add(hblTermsConditionTemplate);

        PageImpl<HblTermsConditionTemplate> hblTermsConditionTemplatePage = new PageImpl<>(hblTermsConditionTemplateList);
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<HblTermsConditionTemplate>, Pageable> pair = fetchData(listReq, HblTermsConditionTemplate.class);

        when(hblTermsConditionTemplateRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(hblTermsConditionTemplatePage);
        assertThrows(RunnerException.class, () -> {
            hblTermsConditionTemplateDao.updateEntityFromSettings(hblTermsConditionTemplateList, 1L, true);
        });
    }

    @Test
    void updateEntityFromSettings() throws RunnerException {
        HblTermsConditionTemplate hblTermsConditionTemplate = new HblTermsConditionTemplate();
        hblTermsConditionTemplate.setIsFrontPrint(true);
        hblTermsConditionTemplate.setId(1L);
        List<HblTermsConditionTemplate> hblTermsConditionTemplateList = new ArrayList<>();
        hblTermsConditionTemplateList.add(hblTermsConditionTemplate);

        PageImpl<HblTermsConditionTemplate> hblTermsConditionTemplatePage = new PageImpl<>(hblTermsConditionTemplateList);
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<HblTermsConditionTemplate>, Pageable> pair = fetchData(listReq, HblTermsConditionTemplate.class);

        when(hblTermsConditionTemplateRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(hblTermsConditionTemplatePage);
        when(hblTermsConditionTemplateRepository.findById(any())).thenReturn(Optional.of(hblTermsConditionTemplate));
        when(hblTermsConditionTemplateRepository.save(any())).thenReturn(hblTermsConditionTemplate);

        assertEquals(hblTermsConditionTemplateList, hblTermsConditionTemplateDao.updateEntityFromSettings(hblTermsConditionTemplateList, 1L, true));
    }

    @Test
    void getTemplateCodeTest() {
        HblTermsConditionTemplate hblTermsConditionTemplate = new HblTermsConditionTemplate();
        hblTermsConditionTemplate.setIsFrontPrint(true);
        hblTermsConditionTemplate.setId(1L);
        List<HblTermsConditionTemplate> hblTermsConditionTemplateList = new ArrayList<>();
        hblTermsConditionTemplateList.add(hblTermsConditionTemplate);

        PageImpl<HblTermsConditionTemplate> hblTermsConditionTemplatePage = new PageImpl<>(hblTermsConditionTemplateList);

        when(hblTermsConditionTemplateRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(hblTermsConditionTemplatePage);
        assertEquals(hblTermsConditionTemplate, hblTermsConditionTemplateDao.getTemplateCode("code", true, "type"));
    }

    @Test
    void getTemplateCodeEmptyTest() {
        List<HblTermsConditionTemplate> hblTermsConditionTemplateList = new ArrayList<>();
        PageImpl<HblTermsConditionTemplate> hblTermsConditionTemplatePage = new PageImpl<>(hblTermsConditionTemplateList);
        when(hblTermsConditionTemplateRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(hblTermsConditionTemplatePage);
        assertEquals(new HblTermsConditionTemplate(), hblTermsConditionTemplateDao.getTemplateCode("code", true, "type"));
    }
}
