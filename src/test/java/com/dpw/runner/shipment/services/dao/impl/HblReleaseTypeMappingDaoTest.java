//package com.dpw.runner.shipment.services.dao.impl;
//
//import static org.junit.jupiter.api.Assertions.assertSame;
//import static org.junit.jupiter.api.Assertions.assertTrue;
//import static org.mockito.ArgumentMatchers.*;
//import static org.mockito.Mockito.verify;
//import static org.mockito.Mockito.when;
//
//import com.dpw.runner.shipment.services.entity.HblReleaseTypeMapping;
//import com.dpw.runner.shipment.services.repository.interfaces.IHblReleaseTypeMappingRepository;
//
//import java.time.LocalDate;
//import java.time.LocalTime;
//import java.util.ArrayList;
//import java.util.List;
//import java.util.Optional;
//import java.util.UUID;
//
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.mockito.Mockito;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.mock.mockito.MockBean;
//import org.springframework.test.context.ContextConfiguration;
//import org.springframework.test.context.junit.jupiter.SpringExtension;
//
//@ContextConfiguration(classes = {HblReleaseTypeMappingDao.class})
//@ExtendWith(SpringExtension.class)
//class HblReleaseTypeMappingDaoTest {
//    @Autowired
//    private HblReleaseTypeMappingDao hblReleaseTypeMappingDao;
//
//    @MockBean
//    private IHblReleaseTypeMappingRepository iHblReleaseTypeMappingRepository;
//
//    /**x
//     * Method under test:
//     * {@link HblReleaseTypeMappingDao#save(HblReleaseTypeMapping)}
//     */
//    @Test
//    void testSave() {
//        // Arrange
//        HblReleaseTypeMapping hblReleaseTypeMapping = new HblReleaseTypeMapping();
//        hblReleaseTypeMapping.setCopiesPrinted(1);
//        hblReleaseTypeMapping.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
//        hblReleaseTypeMapping.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
//        hblReleaseTypeMapping.setGuid(UUID.randomUUID());
//        hblReleaseTypeMapping.setHblId(1L);
//        hblReleaseTypeMapping.setId(1L);
//        hblReleaseTypeMapping.setIsDeleted(true);
//        hblReleaseTypeMapping.setReleaseType("1.0.2");
//        hblReleaseTypeMapping.setTenantId(1);
//        hblReleaseTypeMapping.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
//        hblReleaseTypeMapping.setUpdatedBy("2020-03-01");
//        when(iHblReleaseTypeMappingRepository.save(Mockito.<HblReleaseTypeMapping>any())).thenReturn(hblReleaseTypeMapping);
//
//        HblReleaseTypeMapping hblReleaseTypeMapping2 = new HblReleaseTypeMapping();
//        hblReleaseTypeMapping2.setCopiesPrinted(1);
//        hblReleaseTypeMapping2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
//        hblReleaseTypeMapping2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
//        hblReleaseTypeMapping2.setGuid(UUID.randomUUID());
//        hblReleaseTypeMapping2.setHblId(1L);
//        hblReleaseTypeMapping2.setId(1L);
//        hblReleaseTypeMapping2.setIsDeleted(true);
//        hblReleaseTypeMapping2.setReleaseType("1.0.2");
//        hblReleaseTypeMapping2.setTenantId(1);
//        hblReleaseTypeMapping2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
//        hblReleaseTypeMapping2.setUpdatedBy("2020-03-01");
//
//        // Act
//        HblReleaseTypeMapping actualSaveResult = hblReleaseTypeMappingDao.save(hblReleaseTypeMapping2);
//
//        // Assert
//        verify(iHblReleaseTypeMappingRepository).save(isA(HblReleaseTypeMapping.class));
//        LocalTime expectedToLocalTimeResult = actualSaveResult.getUpdatedAt().toLocalTime();
//        assertSame(expectedToLocalTimeResult, actualSaveResult.getCreatedAt().toLocalTime());
//    }
//
//    /**
//     * Method under test:
//     * {@link HblReleaseTypeMappingDao#findByReleaseTypeAndHblId(Long, String)}
//     */
//    @Test
//    void testFindByReleaseTypeAndHblId() {
//        // Arrange
//        ArrayList<HblReleaseTypeMapping> hblReleaseTypeMappingList = new ArrayList<>();
//        Optional<List<HblReleaseTypeMapping>> ofResult = Optional.of(hblReleaseTypeMappingList);
//        when(iHblReleaseTypeMappingRepository.findByHblIdAndReleaseType(Mockito.<Long>any(), Mockito.<String>any()))
//                .thenReturn(ofResult);
//
//        // Act
//        List<HblReleaseTypeMapping> actualFindByReleaseTypeAndHblIdResult = hblReleaseTypeMappingDao
//                .findByReleaseTypeAndHblId(1L, "1.0.2");
//
//        // Assert
//        verify(iHblReleaseTypeMappingRepository).findByHblIdAndReleaseType(any(), any());
//        assertTrue(actualFindByReleaseTypeAndHblIdResult.isEmpty());
//        assertSame(hblReleaseTypeMappingList, actualFindByReleaseTypeAndHblIdResult);
//    }
//}
