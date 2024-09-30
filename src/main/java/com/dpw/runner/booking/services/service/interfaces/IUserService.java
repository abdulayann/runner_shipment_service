package com.dpw.runner.booking.services.service.interfaces;

import com.dpw.runner.booking.services.dto.request.UsersDto;
import org.springframework.stereotype.Service;

@Service
public interface IUserService {
    UsersDto getUserByToken(String key, String token);
}
