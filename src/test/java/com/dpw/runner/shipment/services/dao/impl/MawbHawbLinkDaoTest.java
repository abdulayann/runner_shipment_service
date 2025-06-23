package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbHawbLinkRepository;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class MawbHawbLinkDaoTest {

    @InjectMocks
    private MawbHawbLinkDao mawbHawbLinkDao;

    @Mock
    private IMawbHawbLinkRepository mawbHawbLinkRepository;

    @Test
    void save() {
        MawbHawbLink mawbHawbLink = MawbHawbLink.builder().build();
        when(mawbHawbLinkRepository.save(any())).thenReturn(mawbHawbLink);
        assertEquals(mawbHawbLink, mawbHawbLinkDao.save(mawbHawbLink));
    }

    @Test
    void findAll() {
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<MawbHawbLink>, Pageable> pair = fetchData(listReq, MawbHawbLink.class);

        MawbHawbLink mawbHawbLink = MawbHawbLink.builder().build();
        List<MawbHawbLink> mawbHawbLinkList = new ArrayList<>();
        mawbHawbLinkList.add(mawbHawbLink);

        PageImpl<MawbHawbLink> mawbHawbLinkPage = new PageImpl<>(mawbHawbLinkList);
        when(mawbHawbLinkRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(mawbHawbLinkPage);

        assertEquals(mawbHawbLinkPage, mawbHawbLinkDao.findAll(pair.getLeft(), pair.getRight()));
    }

    @Test
    void findById() {
        MawbHawbLink mawbHawbLink = MawbHawbLink.builder().build();
        when(mawbHawbLinkRepository.findById(any())).thenReturn(Optional.of(mawbHawbLink));
        assertEquals(mawbHawbLink, mawbHawbLinkDao.findById(1L).get());
    }

    @Test
    void delete() {
        MawbHawbLink mawbHawbLink = MawbHawbLink.builder().build();
        mawbHawbLinkDao.delete(mawbHawbLink);
        verify(mawbHawbLinkRepository, times(1)).delete(mawbHawbLink);
    }

    @Test
    void findByMawbId() {
        MawbHawbLink mawbHawbLink = MawbHawbLink.builder().build();
        List<MawbHawbLink> mawbHawbLinkList = Arrays.asList(mawbHawbLink);
        when(mawbHawbLinkRepository.findByMawbId(any())).thenReturn(mawbHawbLinkList);
        assertEquals(mawbHawbLinkList, mawbHawbLinkDao.findByMawbId(1L));
    }

    @Test
    void findByHawbId() {
        MawbHawbLink mawbHawbLink = MawbHawbLink.builder().build();
        List<MawbHawbLink> mawbHawbLinkList = Arrays.asList(mawbHawbLink);
        when(mawbHawbLinkRepository.findByHawbId(any())).thenReturn(mawbHawbLinkList);
        assertEquals(mawbHawbLinkList, mawbHawbLinkDao.findByHawbId(1L));
    }
}
