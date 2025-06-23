package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.repository.interfaces.IQuartzJobInfoRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class QuartzJobInfoDaoTest {
    @Mock
    private IQuartzJobInfoRepository quartzJobInfoRepository;
    @InjectMocks
    private QuartzJobInfoDao quartzJobInfoDao;

    private QuartzJobInfo quartzJobInfo;

    @BeforeEach
    void setUp() {
        quartzJobInfo = new QuartzJobInfo();
        quartzJobInfo.setId(21L);
        quartzJobInfo.setEntityId(100L);
        quartzJobInfo.setEntityType(Constants.SHIPMENT);
    }

    @Test
    void testFindByJobFilters(){
        when(quartzJobInfoRepository.findByJobFilters(1, 21L, Constants.SHIPMENT)).thenReturn(Optional.of(quartzJobInfo));
        quartzJobInfoDao.findByJobFilters(1, 21L, Constants.SHIPMENT);
        verify(quartzJobInfoRepository, times(1)).findByJobFilters(1, 21L, Constants.SHIPMENT);
    }

    @Test
    void testDelete(){
        quartzJobInfoDao.delete(any());
        verify(quartzJobInfoRepository, times(1)).delete(any());
    }

    @Test
    void testDeleteById(){
        quartzJobInfoDao.deleteById(any());
        verify(quartzJobInfoRepository, times(1)).deleteById(any());
    }

    @Test
    void testFindById(){
        Long quartzId = 21L;
        when(quartzJobInfoRepository.findById(quartzId)).thenReturn(Optional.of(quartzJobInfo));
        Optional<QuartzJobInfo> quartzJobInfo1 = quartzJobInfoDao.findById(quartzId);
        verify(quartzJobInfoRepository, times(1)).findById(quartzId);
        assert(Objects.equals(quartzId, quartzJobInfo1.get().getId()));
    }

    @Test
    void testSave() {
        when(quartzJobInfoRepository.save(any())).thenReturn(quartzJobInfo);
        QuartzJobInfo quartzJobInfo1  = quartzJobInfoDao.save(any());
        assertEquals(quartzJobInfo, quartzJobInfo1);
    }

    @Test
    void testSaveAll() {
        when(quartzJobInfoRepository.saveAll(anyList())).thenReturn(List.of(quartzJobInfo));
        List<QuartzJobInfo> quartzJobInfoList  = quartzJobInfoDao.saveAll(anyList());
        assertEquals(List.of(quartzJobInfo), quartzJobInfoList);
    }

    @Test
    void testFindByIdQuery(){
        Long quartzId = 21L;
        when(quartzJobInfoRepository.findByIdQuery(quartzId)).thenReturn(Optional.of(quartzJobInfo));
        Optional<QuartzJobInfo> quartzJobInfo1 = quartzJobInfoDao.findByIdQuery(quartzId);
        verify(quartzJobInfoRepository, times(1)).findByIdQuery(quartzId);
        assert(Objects.equals(quartzId, quartzJobInfo1.get().getId()));
    }

}